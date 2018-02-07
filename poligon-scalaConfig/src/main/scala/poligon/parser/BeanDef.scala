package poligon.parser

import scala.annotation.compileTimeOnly
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

sealed trait BeanDef[T] {
  @compileTimeOnly("ref method can be used only as constructor or setter argument in BeanDef.")
  def ref: T = throw new NotImplementedError()

  @compileTimeOnly("inline method can be used only as constructor or setter argument in BeanDef.")
  def inline: T = throw new NotImplementedError()

  def cls: Class[T]
}

object BeanDef {

  case class Arg(name: String, value: BeanDef[_])

  case class Constructor[T](cls: Class[T], args: Vector[Arg], setters: Vector[Arg]) extends BeanDef[T]

  case class FactoryMethod[T](cls: Class[T], clsName: String, factoryMethod: String, args: Vector[Arg]) extends BeanDef[T]

  trait SimpleValueDescription

  //TODO: type class for allowed simple values serializable to hocon
  case class SimpleValue[T](cls: Class[T], value: T) extends BeanDef[T]

  //TODO: add can build from any?
  case class ListValue[I, L[_]](cls: Class[L[I]],
                                values: Vector[BeanDef[I]])(implicit val canBuildFrom: CanBuildFrom[Nothing, I, L[I]])
    extends BeanDef[L[I]] {

    def amend[X[_]](other: ListValue[I, X], amend: Boolean = true): ListValue[I, L] = {
      if (amend) {
        ListValue[I, L](cls, values ++ other.values)(canBuildFrom)
      } else {
        ListValue[I, L](cls, other.values)(canBuildFrom)
      }
    }
  }

  object ListValue {
    def empty[I, L[_]](implicit listCls: ClassTag[L[I]], canBuildFrom: CanBuildFrom[Nothing, I, L[I]]): ListValue[I, L] =
      ListValue[I, L](listCls.runtimeClass.asInstanceOf[Class[L[I]]], Vector.empty)(canBuildFrom)

  }

  case class MapValue[K, V, M[_, _]](cls: Class[M[K, V]], value: Map[BeanDef[K], BeanDef[V]]) extends BeanDef[M[K, V]] {

    //TODO: should return MapValue with adjusted valueClass
    @compileTimeOnly("as method can be used only as constructor or setter argument in BeadDef")
    def as[C[_, _] <: scala.collection.Map[_, _]]: C[K, V] = throw new NotImplementedError()

    def amend[X[_, _]](other: MapValue[K, V, X], amend: Boolean = true): MapValue[K, V, M] = {
      if (amend) {
        MapValue[K, V, M](cls, value ++ other.value)
      } else {
        MapValue[K, V, M](cls, other.value)
      }
    }
  }

  case class Referenced[T](cls: Class[T], refName: String, value: BeanDef[T]) extends BeanDef[T]

  case class PropertyValue[T](cls: Class[T], propName: String) extends BeanDef[T]

  case class BeansMap(map: Map[String, BeanDef[_]])

  case class BInstance[T](beanDef: BeanDef[T], instance: T)

  case class BContainer(map: Map[String, BInstance[_]], properties: Map[String, String])

  implicit final class ObjectOps[T](private val t: T) extends AnyVal {
    def toBeanDef: BeanDef[T] = macro poligon.HoconConfigMacros.toBeanDef[T]

    def withSetters(setters: (T => Unit)*): BeanDef[T] = macro poligon.HoconConfigMacros.withSetters[T]
  }

  implicit final class ListOps[I](private val t: List[I]) extends AnyVal {
    def toListValue: ListValue[I, List] = macro poligon.HoconConfigMacros.toListDef
  }

  implicit final class MapOps[K, V](private val m: Map[K, V]) extends AnyVal {
    def toMapValue: MapValue[K, V, Map] = macro poligon.HoconConfigMacros.toBeanDef[Map[K, V]]
  }

  implicit final class StringOps(private val s: String) extends AnyVal {
    def toProp[T](implicit ct: ClassTag[T]): PropertyValue[T] = PropertyValue(ct.runtimeClass.asInstanceOf[Class[T]], s)
  }

  def toBeanDefs[T](holder: T): BeansMap = macro poligon.HoconConfigMacros.toHoconConfig[T]

}