package poligon.parser

import scala.annotation.compileTimeOnly
import scala.reflect.ClassTag


object BeanDef {

  sealed trait BeanDef[T] {
    @compileTimeOnly("ref method can be used only as constructor or setter argument in BeanDef.")
    def ref: T = throw new NotImplementedError()

    @compileTimeOnly("inline method can be used only as constructor or setter argument in BeanDef.")
    def inline: T = throw new NotImplementedError()
  }

  case class Arg(name: String, value: BeanDef[_])

  case class Constructor[T](clsName: String, args: Vector[Arg], setters: Vector[Arg]) extends BeanDef[T]

  case class FactoryMethod[T](clsName: String, factoryMethod: String, args: Vector[Arg]) extends BeanDef[T]

  trait SimpleValueType[T]

  case class SimpleValue[T](value: T) extends BeanDef[T]

  case class ListValue[I, L[_]](values: Vector[BeanDef[I]]) extends BeanDef[L[I]] {

    //TODO: should return ListValue with adjusted valueClass
    @compileTimeOnly("as method can be used only as constructor or setter argument in BeadDef")
    def as[C[_] <: TraversableOnce[_]]: C[I] = throw new NotImplementedError()

    def amend[X[_]](other: ListValue[I, X], amend: Boolean = true): ListValue[I, L] = {
      if (amend) {
        ListValue(values ++ other.values)
      } else {
        ListValue(other.values)
      }
    }
  }

  object ListValue {
    def empty[I, L[_]](implicit listCls: ClassTag[L[_]]): ListValue[I, L] = ListValue[I, L](Vector.empty)
  }

  case class MapValue[K, V, M[_, _]](value: Map[BeanDef[K], BeanDef[V]]) extends BeanDef[M[K, V]] {

    //TODO: should return MapValue with adjusted valueClass
    @compileTimeOnly("as method can be used only as constructor or setter argument in BeadDef")
    def as[C[_, _] <: scala.collection.Map[_, _]]: C[K, V] = throw new NotImplementedError()

    def amend[X[_, _]](other: MapValue[K, V, X], amend: Boolean = true): MapValue[K, V, M] = {
      if (amend) {
        MapValue(value ++ other.value)
      } else {
        MapValue(other.value)
      }
    }
  }

  case class Referenced[T](refName: String, value: BeanDef[T]) extends BeanDef[T]

  case class PropertyValue(propName: String) extends BeanDef[String]

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
    def toProp: PropertyValue = PropertyValue(s)
  }

  def toBeanDefs[T](holder: T): BeansMap = macro poligon.HoconConfigMacros.toHoconConfig[T]

}