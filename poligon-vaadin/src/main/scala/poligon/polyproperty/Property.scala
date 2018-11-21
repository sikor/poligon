package poligon.polyproperty

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait Property[T]

object Property {

  class SimpleProperty[T](var value: T) extends Property[T]

  class RecordProperty[T](val fields: mutable.LinkedHashMap[String, PropertyWithCodec[_]]) extends Property[T]

  class UnionProperty[T](var caseName: String, var value: PropertyWithCodec[_ <: T]) extends Property[T]

  class SeqProperty[E](val value: ArrayBuffer[PropertyWithCodec[E]]) extends Property[Seq[E]]

  class PropertyWithCodec[T](val property: Property[T], val codec: PropertyCodec[T], var lastValue: T) {
    def update(newValue: T): Unit = {
      codec.updateProperty(newValue, property.asInstanceOf[codec.PropertyType])
      lastValue = newValue
    }
  }


  def print(property: Property[_]): String = {
    property match {
      case s: SimpleProperty[_] => s.value.toString
      case r: RecordProperty[_] => "(" + r.fields.map { case (name, value) => s"$name -> ${print(value.property)}" }.mkString(", ") + ")"
      case u: UnionProperty[_] => s"${u.caseName}: ${print(u.value.property)}"
      case s: SeqProperty[_] => "[" + s.value.map(p => print(p.property)).mkString(", ") + "]"
    }
  }

}
