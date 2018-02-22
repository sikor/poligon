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
  val ConstructorCC = q"$BeanDefObj.Constructor"
  val FactoryMethodCC = q"$BeanDefObj.FactoryMethod"
  val SimpleValueCC = q"$BeanDefObj.SimpleValue"
  val ListValueCC = q"$BeanDefObj.ListValue"
  val MapValueCC = q"$BeanDefObj.MapValue"
  val ArgCC = q"$BeanDefObj.Arg"
  val SetterCC = q"$BeanDefObj.Setter"
  val ReferencedCC = q"$BeanDefObj.Referenced"

  val ThisPkg = q"_root_.poligon"

  class BeanDefSignature {
    def unapply(s: Symbol): Opt[MethodSymbol] = {
      if (s.isMethod) {
        val methodSymbol = s.asMethod
        if (methodSymbol.typeSignature.paramLists.isEmpty && methodSymbol.returnType <:< BeanDefTpe && methodSymbol.isPublic) {
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
    q"scala.collection.immutable.Map(..$beans)"
  }

  def getBeanInstance[T](instances: Tree): Tree = {
    val q"""$_($prefix.$methodName)""" = c.prefix.tree
    val methodNameString = methodName.toString()
    q"$instances($methodNameString)"
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

  def withSetters[T: c.WeakTypeTag](setters: Tree*): Tree = {
    val settersArgVector = setters.map { setter =>
      val q"_.$setterName($value)" = setter
      val setterNameString = setterName.toString()
      val valueBeanDef = convertToBean(value)
      q"$SetterCC($setterNameString, $valueBeanDef)"
    }
    val prefix = c.prefix.tree
    q"$prefix.addSetters(Vector(..$settersArgVector))"
  }

  object Constructor {
    def unapply(arg: Tree): Option[(Tree, Tree)] = arg match {
      case q"new $classIdent(...$args)" =>
        if (!classIdent.symbol.owner.isPackage && !classIdent.symbol.owner.isModuleClass) {
          throw new IllegalArgumentException(s"Cannot create beans for classes not directly inside package or object, but got class inside: ${classIdent.symbol.owner.fullName}")
        }
        val a = args.asInstanceOf[List[List[Tree]]].flatten
        val constructor = findMethodForArgs(classIdent, _.isConstructor, a)
        val argsVector = toArgsVector(constructor, a)
        Option(q"classOf[${arg.tpe}]", argsVector)
      case _ => None
    }
  }

  private def convertToBean(arg: Tree): Tree = {
    val cls: Tree = arg match {
      case l: Literal =>
        l.value.value match {
          case _: String => q"classOf[java.lang.String]"
          case _ => q"($arg).getClass"
        }
      case t if t.tpe.typeSymbol.isModuleClass =>
        //check if tree is singleton object and handle it correctly
        val tpeName = singleValueFor(t.tpe)
        q"$tpeName.get.getClass.asInstanceOf[java.lang.Class[${arg.tpe}]]"
      case _ =>
        q"classOf[${arg.tpe}]"
    }
    arg match {
      case Constructor(_, argsVector) =>
        q"$ConstructorCC[${arg.tpe}]($cls, $argsVector, Map.empty)"
      case l: Literal =>
        q"$SimpleValueCC($cls, $l)"
      case q"""$something.this.$refName.ref""" =>
        val refNameStr = refName.toString()
        q"$ReferencedCC($cls, $refNameStr, $something.this.$refName)"
      case q"""$refName.inline""" =>
        q"$refName"
      case q"scala.collection.immutable.List.apply[$itemTpe](..$items)" =>
        val argsTrees = items.asInstanceOf[List[Tree]].map(convertToBean)
        q"$ListValueCC[$itemTpe, scala.collection.immutable.List]($cls, scala.collection.immutable.Vector(..$argsTrees))"
      case q"scala.Predef.Map.apply[$keyTpe, $valueTpe](..$pairs)" =>
        val convertedPairs = pairs.map {
          case q"scala.Predef.ArrowAssoc[$_]($key).->[$_]($value)" => q"${convertToBean(key)} -> ${convertToBean(value)}"
        }
        q"$MapValueCC[$keyTpe, $valueTpe, scala.collection.immutable.Map]($cls, scala.collection.immutable.Vector(..$convertedPairs))"
      case q"""$obj.$staticMethod(...$args)""" if obj.tpe.typeSymbol.isModuleClass => //strangely it works also for java static methods.
        val className = obj.tpe.typeSymbol.fullName.stripSuffix(".type")
        val factoryMethodName = staticMethod.toString()
        val argsFlat = args.asInstanceOf[List[List[Tree]]].flatten
        val met = findMethodForArgs(obj, _.name.toString == staticMethod.toString(), argsFlat)
        val argsVector = toArgsVector(met, argsFlat)
        q"$FactoryMethodCC[${arg.tpe}]($cls, $className, $factoryMethodName, $argsVector)"
      case _ => throw new IllegalArgumentException(s"${arg.toString()}, ${showRaw(arg)}")
    }
  }

  private def findMethodForArgs(tpeOfClass: Tree, filter: Symbol => Boolean, a: List[Tree]): MethodSymbol = {
    require(tpeOfClass.tpe != null, s"type of class not recognized, ident: $tpeOfClass, args: $a")
    val argsTypes = a.map(_.tpe)
    tpeOfClass.tpe.members.find { member =>
      if (filter(member)) {
        val params = paramsTypes(member)
        params.size == argsTypes.size && params.zip(argsTypes).forall {
          case (t1, t2) => t2.erasure.<:<(t1.erasure)
        }
      }
      else {
        false
      }
    }.map(_.asMethod).getOrElse(c.abort(c.enclosingPosition, "Method for args not found"))
  }

  private def paramsTypes[T: c.WeakTypeTag](member: Symbol) = {
    member.asMethod.paramLists.flatMap(_.map(t => t.typeSignature))
  }

}
