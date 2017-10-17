package poligon

import com.avsystem.commons.macros.MacroCommons
import com.avsystem.commons.misc.Opt

import scala.reflect.macros.blackbox


class HoconConfigMacros(val c: blackbox.Context) extends MacroCommons {

  import c.universe._

  val NewLineStringTree: Tree = reify("\n").tree
  val EmptyStringTree: Tree = reify("").tree

  val ParserPkg = q"_root_.poligon.parser"
  val BeanDefObj = q"$ParserPkg.BeanDef"
  val BeanDefTpe: c.universe.Type = getType(tq"$BeanDefObj.BeanDef[_]")
  val ConstructorCC = q"$BeanDefObj.Constructor"
  val FactoryMethodCC = q"$BeanDefObj.FactoryMethod"
  val SimpleValueCC = q"$BeanDefObj.SimpleValue"
  val ListValueCC = q"$BeanDefObj.ListValue"
  val MapValueCC = q"$BeanDefObj.MapValue"
  val ArgCC = q"$BeanDefObj.Arg"
  val ReferencedCC = q"$BeanDefObj.Referenced"
  val BeansMapCC = q"$BeanDefObj.BeansMap"

  val ThisPkg = q"_root_.poligon"

  class BeanDefSignature {
    def unapply(s: Symbol): Opt[MethodSymbol] = {
      if (s.isMethod) {
        val methodSymbol = s.asMethod
        if (methodSymbol.typeSignature.paramLists.isEmpty && methodSymbol.returnType <:< BeanDefTpe) {
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
        val name = m.name.toString
        q"($name, $holder.${m.name})"
    }
    q"$BeansMapCC(scala.collection.immutable.Map(..$beans))"
  }

  def toListDef: Tree = {
    val q"""$_($listDef)""" = c.prefix.tree
    getArgValue(listDef)
  }

  /**
    * @return Vector[Arg]
    */
  private def getArgsVector(method: MethodSymbol, args: List[Tree]): Tree = {
    val argsNames = method.paramLists.flatten.map(p => p.asTerm.name.toString)
    val argsVec = argsNames.zip(args.map(getArgValue)).map {
      case (name, value) =>
        q"$ArgCC($name, $value)"
    }
    q"Vector(..$argsVec)"
  }


  def toBeanDef[T: c.WeakTypeTag]: Tree = {
    val q"$_($value)" = c.prefix.tree
    getArgValue(value)
  }

  private def getArgValue(arg: Tree): Tree = {
    arg match {
      case q"new $classIdent(...$args)" =>
        val clsName = classIdent.symbol.fullName
        val a = args.asInstanceOf[List[List[Tree]]].flatten
        val constructor = findMethodForArgs(classIdent.tpe, _.isConstructor, a)
        val argsVector = getArgsVector(constructor, a)
        q"$ConstructorCC($clsName, $argsVector)"
      case l: Literal =>
        l.value.value match {
          case s: String =>
            val escaped = l.toString()
            q"$SimpleValueCC($escaped)"
          case _ => q"$SimpleValueCC($l)"
        }
      case q"""$something.this.$refName.ref""" =>
        val refNameStr = refName.toString()
        q"$ReferencedCC($refNameStr, $something.this.$refName)"
      case q"""$something.this.$refName.inline""" =>
        q"$something.this.$refName"
      case q"scala.collection.immutable.List.apply[$_](..$items)" =>
        val argsTrees = items.asInstanceOf[List[Tree]].map(getArgValue)
        q"$ListValueCC(scala.collection.immutable.Vector(..$argsTrees))"
      case q"""$listDef.as[$_]""" =>
        q"$listDef"
      case q"""$obj.$staticMethod(...$args)""" if obj.tpe.typeSymbol.isModuleClass => //strangely it works also for java static methods.
        val className = obj.tpe.toString.stripSuffix(".type")
        val factoryMethodName = staticMethod.toString()
        val argsFlat = args.asInstanceOf[List[List[Tree]]].flatten
        val met = findMethodForArgs(obj.tpe, _.name.toString == staticMethod.toString(), argsFlat)
        val argsVector = getArgsVector(met, argsFlat)
        q"$FactoryMethodCC($className, $factoryMethodName, $argsVector)"
      case _ => throw new IllegalArgumentException(s"${arg.toString()}, ${showRaw(arg)}")
    }
  }

  private def findMethodForArgs(tpeOfClass: Type, filter: Symbol => Boolean, a: List[Tree]): MethodSymbol = {
    val argsTypes = a.map(_.tpe)
    tpeOfClass.members.find { member =>
      if (filter(member)) {
        val params = paramsTypes(member)
        params.size == argsTypes.size && params.zip(argsTypes).forall {
          case (t1, t2) => t2.erasure.<:<(t1.erasure)
        }
      } else {
        false
      }
    }.map(_.asMethod).getOrElse(c.abort(c.enclosingPosition, "Method for args not found"))
  }

  private def paramsTypes[T: c.WeakTypeTag](member: Symbol) = {
    member.asMethod.paramLists.flatMap(_.map(t => t.typeSignature))
  }

}
