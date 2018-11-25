package poligon.polyproperty

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait Property[T]

object Property {

  class SimpleProperty[T](var value: T) extends Property[T]

  class RecordProperty[T](val fields: mutable.LinkedHashMap[String, PropertyWithCodec[_]]) extends Property[T]

  class UnionProperty[T](var caseName: String, var value: PropertyWithCodec[_ <: T]) extends Property[T]

  class SeqProperty[E](val value: ArrayBuffer[PropertyWithCodec[E]]) extends Property[Seq[E]]

}
