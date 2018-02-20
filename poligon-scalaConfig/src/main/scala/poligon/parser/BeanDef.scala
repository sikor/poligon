package poligon.parser

import scala.annotation.compileTimeOnly
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

sealed trait BeanDef[T] {
  @compileTimeOnly("ref method can be used only as constructor or setter argument in BeanDef.")
  def ref: T = throw new NotImplementedError()

  @compileTimeOnly("inline method can be used only as constructor or setter argument in BeanDef.")
  def inline: T = throw new NotImplementedError()

  /**
    * Class of the object created by this bean def
    */
  def cls: Class[T]
}

object BeanDef {

  case class Arg(name: String, value: BeanDef[_])

  case class Setter(name: String, value: BeanDef[_])

  case class Constructor[T](cls: Class[T], args: Vector[Arg], setters: Map[String, Setter]) extends BeanDef[T] {

    /**
      * used in macro
      */
    def addSetters(newSetters: Vector[Setter]): Constructor[T] =
      copy(setters = setters ++ newSetters.map(s => s.name -> s).toMap)

    def withSetters(setters: (T => Unit)*): Constructor[T] = macro poligon.HoconConfigMacros.withSetters[T]
  }

  case class FactoryMethod[T](cls: Class[T], factoryClsName: String, factoryMethod: String, args: Vector[Arg]) extends BeanDef[T]

  trait SimpleValueDescription

  //TODO: type class for allowed simple values serializable to hocon
  case class SimpleValue[T](cls: Class[T], value: T) extends BeanDef[T]

  case class ListValue[I, L[_]](cls: Class[L[I]],
                                values: Vector[BeanDef[_ <: I]])(implicit val canBuildFrom: CanBuildFrom[Nothing, I, L[I]])
    extends BeanDef[L[I]] {

    def amend[X[_]](other: ListValue[I, X], amend: Boolean = true): ListValue[I, L] = {
      if (amend) {
        ListValue[I, L](cls, values ++ other.values)(canBuildFrom)
      } else {
        ListValue[I, L](cls, other.values)(canBuildFrom)
      }
    }

    def to[T[_]](implicit cls: ClassTag[T[I]], cbf: CanBuildFrom[Nothing, I, T[I]]): ListValue[I, T] =
      ListValue[I, T](cls.runtimeClass.asInstanceOf[Class[T[I]]], values)(cbf)
  }

  object ListValue {
    def empty[I, L[_]](implicit listCls: ClassTag[L[I]], canBuildFrom: CanBuildFrom[Nothing, I, L[I]]): ListValue[I, L] =
      ListValue[I, L](listCls.runtimeClass.asInstanceOf[Class[L[I]]], Vector.empty)(canBuildFrom)

  }

  case class MapValue[K, V, M[_, _]](cls: Class[M[K, V]], value: Map[BeanDef[K], BeanDef[V]])
                                    (implicit val canBuildFrom: CanBuildFrom[Nothing, (K, V), M[K, V]])
    extends BeanDef[M[K, V]] {

    def amend[X[_, _]](other: MapValue[K, V, X], amend: Boolean = true): MapValue[K, V, M] = {
      if (amend) {
        MapValue[K, V, M](cls, value ++ other.value)
      } else {
        MapValue[K, V, M](cls, other.value)
      }
    }
  }

  case class Referenced[T](cls: Class[T], refName: String, value: BeanDef[T]) extends BeanDef[T]

  trait PropertyConverter[T] {
    def convert(propValue: String): T
  }

  object PropertyConverter {
    implicit val StringConverter: PropertyConverter[String] = (propValue: String) => propValue
    implicit val IntConverter: PropertyConverter[Int] = (propValue: String) => propValue.toInt
  }

  case class PropertyValue[T](cls: Class[T], propName: String)(implicit val converter: PropertyConverter[T]) extends BeanDef[T]

  implicit final class ObjectOps[T](private val t: T) extends AnyVal {
    def toBeanDef: BeanDef[T] = macro poligon.HoconConfigMacros.toBeanDef[T]

    def toConstructorValue: Constructor[T] = macro poligon.HoconConfigMacros.toBeanDef[Constructor[T]]
  }

  implicit final class ListOps[I](private val t: List[I]) extends AnyVal {
    def toListValue: ListValue[I, List] = macro poligon.HoconConfigMacros.toBeanDef[List[I]]
  }

  implicit final class MapOps[K, V](private val m: Map[K, V]) extends AnyVal {
    def toMapValue: MapValue[K, V, Map] = macro poligon.HoconConfigMacros.toBeanDef[Map[K, V]]
  }

  implicit final class StringOps(private val s: String) extends AnyVal {
    def toProp[T](implicit ct: ClassTag[T], converter: PropertyConverter[T]): PropertyValue[T] =
      PropertyValue(ct.runtimeClass.asInstanceOf[Class[T]], s)
  }

  def toBeanDefs[T](holder: T): Map[String, BeanDef[_]] = macro poligon.HoconConfigMacros.toHoconConfig[T]

}