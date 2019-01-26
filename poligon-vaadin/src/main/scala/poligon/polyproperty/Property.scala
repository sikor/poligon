package poligon
package polyproperty

import com.avsystem.commons.serialization.GenRef
import com.github.ghik.silencer.silent
import poligon.polyproperty.Property.PropertyChange
import poligon.polyproperty.Property.PropertyChange.SeqStructuralChange
import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty.SeqMap.EntryPatch

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait Property[T] {
  def getField[R](ref: GenRef.Creator[T] => GenRef[T, R])(implicit rpc: RecordPropertyCodec[T]): Property[R] =
    SubProperty.getField(this)(ref)

  def getCase[R <: T : ClassTag](implicit upc: UnionPropertyCodec[T]): Opt[Property[R]] =
    SubProperty.getCase[T, R](this)

  @silent
  def getSeq[E](implicit ev: T =:= Seq[E]): Seq[Property[E]] =
    SubProperty.getSeq(this.asInstanceOf[Property[Seq[E]]])

  def listenStructure[E](listener: SeqStructuralChange[E] => Unit)(o: PropertyObservers)(implicit ev: Property[T] =:= Property[Seq[E]]): Unit = {
    o.observe(ev.apply(this), new PropertyObserver[Seq[E]] {
      override def propertyChanged(property: Property[Seq[E]]): Unit = {}

      override def propertyRemoved(property: Property[Seq[E]]): Unit = {}

      override def seqChanged(patch: SeqStructuralChange[_]): Unit = {
        listener(patch.asInstanceOf[SeqStructuralChange[E]])
      }

      def seqMapChanged(patch: PropertyChange.SeqMapStructuralChange[_, _, _]): Unit = {}

      def unionChanged(patch: PropertyChange.UnionChange[Seq[E]]): Unit = {}
    })
  }

  def listen(listener: T => Unit, init: Boolean = false)(o: PropertyObservers)(implicit codec: PropertyCodec[T]): Unit = {
    o.observe(this, new PropertyObserver[T] {
      override def propertyChanged(property: Property[T]): Unit = {
        listener(property.get)
      }

      override def propertyRemoved(property: Property[T]): Unit = {}

      override def seqChanged(patch: SeqStructuralChange[_]): Unit = {}

      def seqMapChanged(patch: PropertyChange.SeqMapStructuralChange[_, _, _]): Unit = {}

      def unionChanged(patch: PropertyChange.UnionChange[T]): Unit = {}
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

  sealed trait PropertyChange {
    def property: Property[_]
  }

  object PropertyChange {

    sealed trait Modification[T]

    case class Removed[T](entry: T) extends Modification[T]

    case class Added[T](entry: T) extends Modification[T]

    class ValueChange(val property: Property[_]) extends PropertyChange

    class SeqMapStructuralChange[K, V, T](val property: Property[T], val modifications: EntryPatch[K, Property[V]]) extends PropertyChange

    type SeqStructuralChange[E] = SeqMapStructuralChange[Int, E, Seq[E]]

    class UnionChange[T](val property: UnionProperty[T], val newValue: Property[_ <: T], val oldValue: Property[_ <: T]) extends PropertyChange

  }

  sealed trait Diff[+T]

  object Diff {

    case object NoOp extends Diff[Nothing]

    case class Val[T](value: T) extends Diff[T]

  }

}
