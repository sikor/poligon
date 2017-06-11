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
    val a = args.asInstanceOf[List[List[Tree]]]
    val tpeOfClass = weakTypeOf[T]
    val constructorArgsTpes = (a.map(_.map(t => t.tpe)) match {
      case Nil => List(List())
      case other => other
    }).flatten
    val mem = tpeOfClass.members.filter(_.isConstructor).map(paramsTypes).mkString(", ")
    val constructor = tpeOfClass.members.filter(member => member.isConstructor && paramsTypes(member).size == constructorArgsTpes.size && paramsTypes(member).zip(constructorArgsTpes).forall {
      case (t1, t2) => t2.erasure.<:<(t1.erasure)
    }).mkString("constructor: ", ", ", s" arg tpes: $constructorArgsTpes, constructors: $mem")
    val beanDef =
      s"""
         |{
         |  %class = $clsName
         |  %constructor-args = {
         |    $constructor
         |  }
         |}
       """.stripMargin
    q"""
       $beanDef
         """

  }

  private def getConstructor[T: c.WeakTypeTag](a: List[List[Tree]]): MethodSymbol = {
    val tpeOfClass = weakTypeOf[T]
    val constructorArgsTpes = (a.map(_.map(t => t.tpe)) match {
      case Nil => List(List())
      case other => other
    }).flatten
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
