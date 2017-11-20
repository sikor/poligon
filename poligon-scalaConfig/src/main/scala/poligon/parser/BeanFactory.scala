package poligon.parser

import poligon.parser.BeanDef.BeanDef

object BeanFactory {
  def createInstance[T](beanDef: BeanDef[T], context: Map[String, Any]): T = ???
}
