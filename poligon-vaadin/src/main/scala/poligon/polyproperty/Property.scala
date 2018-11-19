package poligon.polyproperty

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait Property[T]

object Property {

  class SimpleProperty[T](private var value: T) extends Property[T]

  class CaseProperty[T](private val fields: mutable.Map[String, PropertyWithCodec[_]]) extends Property[T]

  class UnionProperty[T](var caseName: String, var value: PropertyWithCodec[_ <: T]) extends Property[T]

  class ListProperty[T[_], E](private val value: ArrayBuffer[PropertyWithCodec[E]]) extends Property[T[E]]

  class PropertyWithCodec[T](val property: Property[T], val codec: PropertyCodec[T], var lastValue: T) {
    def update(newValue: T): Unit = {
      codec.updateProperty(newValue, property.asInstanceOf[codec.PropertyType])
      lastValue = newValue
    }
  }

}
