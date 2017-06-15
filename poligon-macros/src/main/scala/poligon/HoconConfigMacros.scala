package poligon

import scala.annotation.compileTimeOnly
import scala.reflect.macros.blackbox


object MyMacros {

  implicit final class ObjectOps[T](t: T) {
    def toBeanDef: BeanDef[T] = macro HoconConfigMacros.toBeanDef[T]
  }

  implicit final class ListOps[T](t: List[T]) {
    @compileTimeOnly("as method can be used only as constructor or setter argument in BeadDef")
    def as[I[_] <: TraversableOnce[_]]: I[T] = throw new NotImplementedError()
  }

}

class BeanDef[T](hocon: String) {
  @compileTimeOnly("ref method can be used only as constructor or setter argument in BeanDef.")
  def ref: T = throw new NotImplementedError()

  override def toString: String = hocon
}

class HoconConfigMacros(val c: blackbox.Context) {

  import c.universe._

  val ThisPkg = q"_root_.poligon"
  val BeanDefCls = tq"$ThisPkg.BeanDef"

  val ScalaPkg = q"_root_.scala"
  val CollectionPkg = q"$ScalaPkg.collection"
  val ListObj = q"$CollectionPkg.immutable.List"
  val ListCls = tq"$CollectionPkg.immutable.List"


  def toBeanDef[T: c.WeakTypeTag]: Tree = {
    val q"$_(new $classIdent(...$args))" = c.prefix.tree
    val clsName = classIdent.symbol.fullName
    val a = args.asInstanceOf[List[List[Tree]]].flatten
    val constructor = getConstructor(a)
    val constructorArgsNames = constructor.paramLists.flatten.map(p => p.asTerm.name)
    val constructorMap = constructorArgsNames.zip(a.map(getArgValue)).map {
      case (name, value) => s"$name = $value"
    }.mkString("\n")
    val beanDef =
      s"""
         |{
         |  %class = $clsName
         |  %constructor-args = {
         |    $constructorMap
         |  }
         |}
       """.stripMargin
    q"""
       new $BeanDefCls($beanDef)
         """
  }

  private def getArgValue(arg: Tree): String = {
    arg match {
      case l: Literal => l.value.value match {
        case s: String => s""""${s.replaceAllLiterally("\\", "\\\\").replaceAllLiterally("\n", "\\n").replaceAllLiterally("\"", "\\\"")}""""
        case other => other.toString
      }
      case q"""$_.this.$refName.ref""" => s"{%ref = $refName}"
      case q"""scala.collection.immutable.List.apply[$_](..$items)""" => s"[${items.map(getArgValue).mkString(", ")}]"
      case _ => s"${arg.toString()}, ${showRaw(arg)}"
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
}
