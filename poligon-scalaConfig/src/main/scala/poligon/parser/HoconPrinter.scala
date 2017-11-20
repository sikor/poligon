package poligon.parser

import poligon.parser.BeanDef.{Arg, BeanDef, BeansMap, Constructor, FactoryMethod, ListValue, MapValue, PropertyValue, Referenced, SimpleValue}

object HoconPrinter {

  private def argsToHocon(num: Int, args: Vector[Arg]): String =
    args.map(a => s"${a.name} = ${move(num, toHoconObject(a.value))}").mkString(separator(num))

  private def move(num: Int, value: String): String = value.lines.mkString(separator(num))

  private def separator(num: Int): String = s"\n${" " * num}"

  private def cleanHocon(hocon: String): String = {
    hocon.replaceAll("""\s*%constructor-args = \{\s+\}""", "")
      .lines.filter(l => !l.matches("""\s*""")).mkString("\n")
  }

  def toHoconObject(beanDef: BeanDef[_]): String = beanDef match {
    case Constructor(clsName, args, setters) =>
      cleanHocon(
        s"""{
           |  %class = $clsName
           |  %constructor-args = {
           |    ${argsToHocon(4, args)}
           |  }
           |  ${argsToHocon(2, setters)}
           |}""".stripMargin)
    case FactoryMethod(clsName, factoryMethod, args) =>
      cleanHocon(
        s"""{
           |  %class = $clsName
           |  %factory-method = $factoryMethod
           |  %constructor-args = {
           |    ${argsToHocon(4, args)}
           |  }
           |}""".stripMargin)
    case SimpleValue(value) =>
      value.toString
    case ListValue(values) =>
      values.map(v => toHoconObject(v)).mkString("[", ", ", "]")
    case MapValue(value) =>
      value.map(v => s"  ${move(2, toHoconObject(v._1))} = ${move(2, toHoconObject(v._2))}")
        .mkString("{\n", "\n", "\n}")
    case Referenced(refName, _) =>
      s"{ %ref = $refName }"
    case PropertyValue(propName) =>
      s"$${$propName}"
  }

  def toHocon(beansMap: BeansMap): String =
    beansMap.map.map {
      case (name, bean) =>
        s"""$name = ${toHoconObject(bean)}"""
    }.mkString("\n")
}
