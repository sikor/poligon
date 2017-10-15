package poligon.parser

/**
  * Inline beans are that that can be referenced - rename ref method to something more general like extract
  * Should data be related to some scala type? Or should it be just raw structure description? If related to type it also should
  * take the code that evaluate to its value (by name argument?)
  *
  * BeanDef always represents some type. If this type is a list we should consider adding append method to it and converting
  * it to other types of lists.
  *
  */
object BeanDef {

  private def argsToHocon(num: Int, args: Vector[Arg]): String =
    args.map(a => s"${a.name} = ${move(num, a.value.toHocon)}").mkString(separator(num))

  private def move(num: Int, value: String): String = value.lines.mkString(separator(num))

  private def separator(num: Int): String = s"\n${" " * num}"

  case class Arg(name: String, value: BeanDef[_])

  sealed trait BeanDef[T] {
    def toHocon: String
  }

  case class Constructor[T](clsName: String, args: Vector[Arg]) extends BeanDef[T] {
    def toHocon: String =
      s"""
         |{
         |  %class = $clsName
         |  %constructor-args = {
         |    ${argsToHocon(4, args)}
         |  }
         |}
           """.stripMargin
  }

  case class FactoryMethod[T](clsName: String, factoryMethod: String, args: Vector[Arg]) extends BeanDef[T] {
    def toHocon: String =
      s"""
         |{
         |  %class = $clsName
         |  %factory-method = $factoryMethod
         |  %constructor-args = {
         |    ${argsToHocon(4, args)}
         |  }
         |}
           """.stripMargin
  }

  case class SimpleValue[T](value: T) extends BeanDef[T] {
    def toHocon: String = value.toString
  }

  case class ListValue[I, L[_]](values: Vector[BeanDef[I]]) extends BeanDef[L[I]] {
    def toHocon: String =
      values.map(v => v.toHocon).mkString("[", ", ", "]")
  }

  case class MapValue[K, V, M[_, _]](value: Map[BeanDef[K], BeanDef[V]]) extends BeanDef[M[K, V]] {
    def toHocon: String =
      value.map(v => s"  ${move(2, v._1.toHocon)} = ${move(2, v._2.toHocon)}").mkString("{\n", "\n", "\n}")
  }

}
