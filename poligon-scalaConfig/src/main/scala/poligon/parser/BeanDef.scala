package poligon.parser

import scala.annotation.compileTimeOnly
import scala.reflect.ClassTag

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

  case class Arg(name: String, value: BeanDef[_])

  sealed trait BeanDef[T] {

    @compileTimeOnly("ref method can be used only as constructor or setter argument in BeanDef.")
    def ref: T = throw new NotImplementedError()

    @compileTimeOnly("inline method can be used only as constructor or setter argument in BeanDef.")
    def inline: T = throw new NotImplementedError()

    def createInstance(context: BContainer): T = ???

    def valueClass: Class[_]
  }

  case class Constructor[T](clsName: String, args: Vector[Arg], setters: Vector[Arg]) extends BeanDef[T] {

    def createInstanceByReflection(context: BContainer): T = {
      val cls = getClass.getClassLoader.loadClass(clsName)
      cls.getConstructors.find { c =>
        c.getParameterTypes.zip(args.map(_.value)).forall {
          case (expected, beanDef) =>
            expected.isAssignableFrom(???)
            ???
        }
      }
      ???
    }

    def valueClass: Class[_] = getClass.getClassLoader.loadClass(clsName)
  }

  case class FactoryMethod[T](clsName: String, factoryMethod: String, args: Vector[Arg]) extends BeanDef[T] {

    override def valueClass: Class[_] = {
      getClass.getClassLoader.loadClass(clsName).getMethod(factoryMethod, args.map(_.value.valueClass): _*)
        .getReturnType
    }
  }

  case class SimpleValue[T](value: T) extends BeanDef[T] {
    def toHocon: String = value.toString

    override def valueClass: Class[_] = value.getClass
  }

  case class ListValue[I, L[_]](values: Vector[BeanDef[I]], valueClass: Class[_]) extends BeanDef[L[I]] {

    //TODO: should return ListValue with adjusted valueClass
    @compileTimeOnly("as method can be used only as constructor or setter argument in BeadDef")
    def as[C[_] <: TraversableOnce[_]]: C[I] = throw new NotImplementedError()


    def amend[X[_]](other: ListValue[I, X], amend: Boolean = true): ListValue[I, L] = {
      if (amend) {
        ListValue(values ++ other.values, valueClass)
      } else {
        ListValue(other.values, valueClass)
      }
    }
  }

  object ListValue {
    def empty[I, L[_]](implicit listCls: ClassTag[L[_]]): ListValue[I, L] = ListValue[I, L](Vector.empty, implicitly[ClassTag[L[_]]].runtimeClass)
  }

  case class MapValue[K, V, M[_, _]](value: Map[BeanDef[K], BeanDef[V]], valueClass: Class[_]) extends BeanDef[M[K, V]] {

    //TODO: should return MapValue with adjusted valueClass
    @compileTimeOnly("as method can be used only as constructor or setter argument in BeadDef")
    def as[C[_, _] <: scala.collection.Map[_, _]]: C[K, V] = throw new NotImplementedError()

    def amend[X[_, _]](other: MapValue[K, V, X], amend: Boolean = true): MapValue[K, V, M] = {
      if (amend) {
        MapValue(value ++ other.value, valueClass)
      } else {
        MapValue(other.value, valueClass)
      }
    }
  }

  case class Referenced[T](refName: String, value: BeanDef[T]) extends BeanDef[T] {

    def valueClass: Class[_] = value.valueClass
  }

  case class PropertyValue(propName: String) extends BeanDef[String] {
    def toHocon: String = s"$${$propName}"

    override def valueClass: Class[_] = classOf[String]
  }

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