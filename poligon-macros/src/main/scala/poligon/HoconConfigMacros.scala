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
  val BeanDefTpe: c.universe.Type = getType(tq"$ParserPkg.BeanDef[_]")
  val InstantiableCC = q"$ParserPkg.Instantiable"
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
    convertToBean(listDef)
  }

  /**
    * @return Vector[Arg]
    */
  private def toArgsVector(method: MethodSymbol, args: List[Tree]): Tree = {
    val argsNames = method.paramLists.flatten.map(p => p.asTerm.name.toString)
    val argsVec = argsNames.zip(args.map(convertToBean)).map {
      case (name, value) =>
        q"$ArgCC($name, $value)"
    }
    q"Vector(..$argsVec)"
  }

  private def getPrefixTree: Tree = {
    val q"$_($value)" = c.prefix.tree
    value.asInstanceOf[Tree]
  }

  def toBeanDef[T: c.WeakTypeTag]: Tree =
    convertToBean(getPrefixTree)

  private def toBeanPropertyName(setterName: String): String = setterName.substring(3, 4).toLowerCase + setterName.substring(4)

  def withSetters[T: c.WeakTypeTag](setters: Tree*): Tree = {
    val settersArgVector = setters.map { setter =>
      val q"_.$setterName($value)" = setter
      val setterNameString = toBeanPropertyName(setterName.toString())
      val valueBeanDef = convertToBean(value)
      q"$ArgCC($setterNameString, $valueBeanDef)"
    }
    val Constructor(clsName, argsVector) = getPrefixTree
    q"$ConstructorCC($clsName, $argsVector, Vector(..$settersArgVector))"
  }

  object Constructor {
    def unapply(arg: Tree): Option[(String, Tree)] = arg match {
      case q"new $classIdent(...$args)" =>
        val clsName = classIdent.symbol.fullName
        if (!classIdent.symbol.owner.isPackage) {
          throw new IllegalArgumentException(s"Cannot create beans for classes not directly inside package, but got class inside: ${classIdent.symbol.owner.fullName}")
        }
        val a = args.asInstanceOf[List[List[Tree]]].flatten
        val constructor = findMethodForArgs(classIdent.tpe, _.isConstructor, a)
        val argsVector = toArgsVector(constructor, a)
        Option(clsName, argsVector)
      case _ => None
    }
  }

  private def convertToBean(arg: Tree): Tree = {
    val beanDef = arg match {
      case Constructor(clsName, argsVector) =>
        q"$ConstructorCC($clsName, $argsVector, Vector.empty)"
      case l: Literal =>
        q"$SimpleValueCC($l)"
      case q"""$something.this.$refName.ref""" =>
        val refNameStr = refName.toString()
        q"$ReferencedCC($refNameStr, $something.this.$refName)"
      case q"""$refName.inline""" =>
        q"$refName"
      case q"scala.collection.immutable.List.apply[$_](..$items)" =>
        val argsTrees = items.asInstanceOf[List[Tree]].map(convertToBean)
        q"$ListValueCC(scala.collection.immutable.Vector(..$argsTrees))"
      case q"""$listDef.as[$_]""" =>
        q"$listDef"
      case q"scala.Predef.Map.apply[$_, $_](..$pairs)" =>
        val convertedPairs = pairs.map {
          case q"scala.Predef.ArrowAssoc[$_]($key).->[$_]($value)" => q"${convertToBean(key)} -> ${convertToBean(value)}"
        }
        q"$MapValueCC(scala.collection.immutable.Map.apply(..$convertedPairs))"
      case q"""$obj.$staticMethod(...$args)""" if obj.tpe.typeSymbol.isModuleClass => //strangely it works also for java static methods.
        val className = obj.tpe.toString.stripSuffix(".type")
        val factoryMethodName = staticMethod.toString()
        val argsFlat = args.asInstanceOf[List[List[Tree]]].flatten
        val met = findMethodForArgs(obj.tpe, _.name.toString == staticMethod.toString(), argsFlat)
        val argsVector = toArgsVector(met, argsFlat)
        q"$FactoryMethodCC($className, $factoryMethodName, $argsVector)"
      case _ => throw new IllegalArgumentException(s"${arg.toString()}, ${showRaw(arg)}")
    }
    val cls: Tree = arg match {
      case l: Literal =>
        l.value.value match {
          case _: String => q"classOf[java.lang.String]"
          case _ => q"($arg).getClass"
        }
      case _ => q"classOf[${arg.tpe}]"
    }
    q"$InstantiableCC($cls, $beanDef)"
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
