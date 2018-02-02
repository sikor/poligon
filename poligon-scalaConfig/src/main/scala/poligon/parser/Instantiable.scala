package poligon.parser

import scala.annotation.compileTimeOnly


case class Instantiable[T](cls: Class[T], beanDef: BeanDef[T]) {
  @compileTimeOnly("ref method can be used only as constructor or setter argument in BeanDef.")
  def ref: T = throw new NotImplementedError()

  @compileTimeOnly("inline method can be used only as constructor or setter argument in BeanDef.")
  def inline: T = throw new NotImplementedError()
}
