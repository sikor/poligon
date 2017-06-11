import scala.reflect.macros.blackbox


object MyMacros {

  implicit final class ObjectOps[T](t: T) {
    def toHoconConfig: String = macro HoconConfigMacros.toHoconConfig[T]
  }

}

class HoconConfigMacros(val c: blackbox.Context) {

  import c.universe._


  def toHoconConfig[T: c.WeakTypeTag]: Tree = {
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
       $beanDef
         """

  }

  private def getArgValue(arg: Tree): String = {
    val t = arg.tpe
    arg match {
      case l: Literal => l.value.value match {
        case s: String => s""""${s.replaceAllLiterally("\\", "\\\\").replaceAllLiterally("\n", "\\n").replaceAllLiterally("\"", "\\\"")}""""
        case other => other.toString
      }
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
