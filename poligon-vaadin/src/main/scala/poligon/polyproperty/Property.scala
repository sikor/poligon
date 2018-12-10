package poligon.polyproperty

import com.avsystem.commons.serialization.GenRef
import poligon.polyproperty.PropertyObserver.{PropertyObservers, SeqPatch}
import poligon.{ClassTag, Opt}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait Property[T] {
  def get[R](ref: GenRef.Creator[T] => GenRef[T, R])(implicit rpc: RecordPropertyCodec[T]): Property[R] =
    SubProperty.getField(this)(ref)

  def getCase[R <: T : ClassTag](implicit upc: UnionPropertyCodec[T]): Opt[Property[R]] =
    SubProperty.getCase[T, R](this)

  def getSeq[E](implicit ev: Property[T] =:= Property[Seq[E]]): Seq[Property[E]] =
    SubProperty.getSeq(ev.apply(this))

  def listenStructure[E](listener: SeqPatch[E] => Unit)(implicit ev: Property[T] =:= Property[Seq[E]], o: PropertyObservers): Unit = {
    o.observe(ev.apply(this), new PropertyObserver[Seq[E]] {
      override def propertyChanged(property: Property[Seq[E]]): Unit = {}

      override def propertyRemoved(property: Property[Seq[E]]): Unit = {}

      override def seqChanged(patch: SeqPatch[_]): Unit = {
        listener(patch.asInstanceOf[SeqPatch[E]])
      }
    })
  }

  def listen(listener: T => Unit, init: Boolean = false)(implicit o: PropertyObservers, codec: PropertyCodec[T]): Unit = {
    o.observe(this, new PropertyObserver[T] {
      override def propertyChanged(property: Property[T]): Unit = {
        listener(property.getValue)
      }

      override def propertyRemoved(property: Property[T]): Unit = {}

      override def seqChanged(patch: SeqPatch[_]): Unit = {}
    })
    if (init) {
      listener(getValue)
    }
  }

  def getValue(implicit codec: PropertyCodec[T]): T = codec.readProperty(this.asInstanceOf[codec.PropertyType])

}

object Property {

  class SimpleProperty[T](var value: T) extends Property[T]

  class RecordProperty[T](val fields: mutable.LinkedHashMap[String, Property[_]]) extends Property[T]

  class UnionProperty[T](var caseName: String, var value: Property[_ <: T]) extends Property[T]

  class SeqProperty[E](val value: ArrayBuffer[Property[E]]) extends Property[Seq[E]]

  def print(property: Property[_]): String = {
    property match {
      case s: SimpleProperty[_] => s.value.toString
      case r: RecordProperty[_] => "(" + r.fields.map { case (name, value) => s"$name -> ${print(value)}" }.mkString(", ") + ")"
      case u: UnionProperty[_] => s"${u.caseName}: ${print(u.value)}"
      case s: SeqProperty[_] => "[" + s.value.map(p => print(p)).mkString(", ") + "]"
    }
  }

}
