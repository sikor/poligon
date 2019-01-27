package poligon
package polyproperty

import poligon.polyproperty.PropertyCodec.StructuralPropertyCodec.StructuralChange
import poligon.polyproperty.PropertyObserver.PropertyObservers

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait Property[T] {

  def listenStructure[E](listener: StructuralChange[Int, E, Seq[E]] => Unit)(o: PropertyObservers)(implicit ev: Property[T] =:= Property[Seq[E]]): Unit = {
    o.observe(ev.apply(this), new PropertyObserver[Seq[E]] {
      override def propertyChanged(property: Property[Seq[E]]): Unit = {}

      override def propertyRemoved(property: Property[Seq[E]]): Unit = {}

      override def structureChange(patch: StructuralChange[_, _, Seq[E]]): Unit = {
        listener(patch.asInstanceOf[StructuralChange[Int, E, Seq[E]]])
      }
    })
  }

  def listen(listener: T => Unit, init: Boolean = false)(o: PropertyObservers)(implicit codec: PropertyCodec[T]): Unit = {
    o.observe(this, new PropertyObserver[T] {
      override def propertyChanged(property: Property[T]): Unit = {
        listener(property.get)
      }

      override def propertyRemoved(property: Property[T]): Unit = {}

      override def structureChange(patch: StructuralChange[_, _, T]): Unit = {}
    })
    if (init) {
      listener(get)
    }
  }

  def get(implicit codec: PropertyCodec[T]): T = codec.readProperty(this.asInstanceOf[codec.PropertyType])

  def obs(implicit codec: PropertyCodec[T]): Obs[T] = new PropertyObs[T](this, codec)

  def map[R](f: T => R)(implicit codec: PropertyCodec[T]): Obs[R] = obs.map(f)
}

object Property {

  class SimpleProperty[T](var value: T) extends Property[T]

  class RecordProperty[T](val fields: mutable.LinkedHashMap[String, Property[_]]) extends Property[T]

  class UnionProperty[T](var caseName: String, var value: Property[_ <: T]) extends Property[T]

  class SeqProperty[E](val value: ArrayBuffer[Property[E]]) extends Property[Seq[E]]

  class SeqMapProperty[K, V, T](val value: SeqMap[K, Property[V]]) extends Property[T]

  class SortedMapProperty[K, V, T](val value: SeqSortedMap[K, Property[V]]) extends Property[T]

  type MapProperty[K, V] = SeqMapProperty[K, V, BMap[K, V]]

  def print(property: Property[_]): String = {
    property match {
      case s: SimpleProperty[_] => s.value.toString
      case r: RecordProperty[_] => "(" + r.fields.map { case (name, value) => s"$name -> ${print(value)}" }.mkString(", ") + ")"
      case u: UnionProperty[_] => s"${u.caseName}: ${print(u.value)}"
      case s: SeqProperty[_] => "[" + s.value.map(p => print(p)).mkString(", ") + "]"
      case s: SeqMapProperty[_, _, _] => "[" + s.value.map(p => s"${p._1} -> ${print(p._2)}").mkString(", ") + "]"
      case s: SortedMapProperty[_, _, _] => "[" + s.value.map(p => s"${p._1} -> ${print(p._2)}").mkString(", ") + "]"
    }
  }

  sealed trait Diff[+T]

  object Diff {

    case object NoOp extends Diff[Nothing]

    case class Val[T](value: T) extends Diff[T]

  }

}
