package poligon

import com.avsystem.commons.misc.Opt

import scala.annotation.compileTimeOnly
import scala.reflect.macros.blackbox


object MyMacros {

  implicit final class ObjectOps[T](t: T) {
    def toBeanDef: BeanDef[T] = macro HoconConfigMacros.toBeanDef[T]
  }

  implicit final class ListOps[T](t: List[T]) {
    def toListDef: ListDef[T] = macro HoconConfigMacros.toListDef[T]

    def toAppendDef: AppendDef[T] = macro HoconConfigMacros.toAppendDef[T]
  }

  def toHoconConfig[T](holder: T): String = macro HoconConfigMacros.toHoconConfig[T]

}

class BeanDef[T](val hocon: String) {
  @compileTimeOnly("ref method can be used only as constructor or setter argument in BeanDef.")
  def ref: T = throw new NotImplementedError()

  override def toString: String = hocon
}

sealed trait HoconList[T]

class ListDef[T](val items: String*) extends HoconList[T] {

  @compileTimeOnly("as method can be used only as constructor or setter argument in BeadDef")
  def as[I[_] <: TraversableOnce[_]]: I[T] = throw new NotImplementedError()

  def toHocon: String = items.mkString("[", ", ", "]")

  def amend(other: HoconList[T]): ListDef[T] = {
    other match {
      case l: ListDef[T] => l
      case a: AppendDef[T] => new ListDef(items ++ a.items: _*)
    }
  }
}


object ListDef {
  def empty[T] = new ListDef[T]()
}

class AppendDef[T](val items: String*) extends HoconList[T]


object AppendDef {
  def empty[T] = new AppendDef[T]()
}

class HoconConfigMacros(val c: blackbox.Context) {

  import c.universe._

  val NewLineStringTree: Tree = reify("\n").tree
  val EmptyStringTree: Tree = reify("").tree

  val ThisPkg = q"_root_.poligon"
  val BeanDefCls = tq"$ThisPkg.BeanDef"
  val ListDefCls = tq"$ThisPkg.ListDef"
  val AppendDefCls = tq"$ThisPkg.AppendDef"

  val ScalaPkg = q"_root_.scala"
  val CollectionPkg = q"$ScalaPkg.collection"
  val ListObj = q"$CollectionPkg.immutable.List"
  val ListCls = tq"$CollectionPkg.immutable.List"

  class BeanDefSignature {
    def unapply(s: Symbol): Opt[MethodSymbol] = {
      if (s.isMethod) {
        val methodSymbol = s.asMethod
        if (methodSymbol.typeSignature.paramLists.isEmpty && methodSymbol.returnType <:< typeOf[BeanDef[_]]) {
          Opt(methodSymbol)
        } else {
          Opt.Empty
        }
      } else {
        Opt.Empty
      }
    }
  }

  def toHoconConfig[T: c.WeakTypeTag](holder: Tree): Tree = {
    val tpeOfClass = weakTypeOf[T]
    val beanDefSignature = new BeanDefSignature
    val beans = tpeOfClass.members.collect {
      case beanDefSignature(m) =>
        q""" ${s"${m.name} = "} + $holder.${m.name}.hocon"""
    }.toList

    joinStringTrees(beans, NewLineStringTree)
  }

  def toListDef[T: c.WeakTypeTag]: Tree = {
    val q"""$_(scala.collection.immutable.List.apply[$_](..$items))""" = c.prefix.tree
    val hoconItems = items.asInstanceOf[List[Tree]].map(getArgValue)
    q"""
      new $ListDefCls(..$hoconItems)
     """
  }

  def toAppendDef[T: c.WeakTypeTag]: Tree = {
    val q"""$_(scala.collection.immutable.List.apply[$_](..$items))""" = c.prefix.tree
    val hoconItems = items.asInstanceOf[List[Tree]].map(getArgValue)
    q"""
      new $AppendDefCls(..$hoconItems)
     """
  }

  def toBeanDef[T: c.WeakTypeTag]: Tree = {
    val q"$_(new $classIdent(...$args))" = c.prefix.tree
    val clsName = classIdent.symbol.fullName
    val a = args.asInstanceOf[List[List[Tree]]].flatten
    val constructor = getConstructor(a)
    val constructorArgsNames = constructor.paramLists.flatten.map(p => p.asTerm.name)
    val constructorMap = joinStringTrees(constructorArgsNames.zip(a.map(getArgValue)).map {
      case (name, value) =>
        trees"""$name = $value"""
    }, NewLineStringTree)
    val beanDef =
      trees"""{
           %class = $clsName
           %constructor-args = {
             $constructorMap
           }
      }"""
    q"""
       new $BeanDefCls($beanDef)
         """
  }

  private def getArgValue(arg: Tree): Tree = {
    arg match {
      case l: Literal => l.value.value match {
        case s: String =>
          val str = "\"" + s.replaceAllLiterally("\\", "\\\\").replaceAllLiterally("\n", "\\n").replaceAllLiterally("\"", "\\\"") + "\""
          q"$str"
        case other => q"${other.toString}"
      }
      case q"""$_.this.$refName.ref""" => q"""${s"{%ref = $refName}"}"""
      case q"""_root_.scala.collection.immutable.List.apply[$_](..$items)""" =>
        val argsTrees = items.asInstanceOf[List[Tree]].map(getArgValue)
        joinStringTressToHoconList(argsTrees)
      case q"""$listDef.as[$_]""" => q"$listDef.toHocon"
      case _ => q"""${s"${arg.toString()}, ${showRaw(arg)}"}"""
    }
  }

  private def getConstructor[T: c.WeakTypeTag](a: List[Tree]): MethodSymbol = {
    val tpeOfClass = weakTypeOf[T]
    val constructorArgsTpes = a.map(_.tpe)
    tpeOfClass.members.find { member =>
      if (member.isConstructor) {
        val params = paramsTypes(member)
        params.size == constructorArgsTpes.size && params.zip(constructorArgsTpes).forall {
          case (t1, t2) => t2.erasure.<:<(t1.erasure)
        }
      } else {
        false
      }
    }.map(_.asMethod).getOrElse(c.abort(c.enclosingPosition, "Constructor for args not found"))
  }

  private def paramsTypes[T: c.WeakTypeTag](member: Symbol) = {
    member.asMethod.paramLists.flatMap(_.map(t => t.typeSignature))
  }

  def joinStringTrees(trees: List[Tree], separator: Tree): Tree = {
    q"""$ListObj[String](..$trees).mkString($separator)"""
  }

  def joinStringTressToHoconList(trees: List[Tree]): Tree = {
    q"""$ListObj[String](..$trees).mkString("[", ", ", "]")"""
  }

  implicit class TreeList(val sc: StringContext) {
    def trees(args: Any*): Tree = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      var buf = List.newBuilder[Tree]
      while (strings.hasNext) {
        buf += q"${strings.next}"
        if (expressions.hasNext) {
          buf += toTree(expressions.next())
        }
      }
      joinStringTrees(buf.result(), EmptyStringTree)
    }

    private def toTree(value: Any): Tree = {
      value match {
        case t: Tree => t
        case s => q"${s.toString}"
      }
    }
  }

}
