package poligon.polyproperty

import com.avsystem.commons.serialization.GenRef
import com.github.ghik.silencer.silent
import poligon.polyproperty.PropertyObserver.{PropertyObservers, SeqPatch}
import poligon.{ClassTag, Opt}

import scala.collection.mutable

sealed trait Property[T] {
  def getField[R](ref: GenRef.Creator[T] => GenRef[T, R])(implicit rpc: RecordPropertyCodec[T]): Property[R] =
    SubProperty.getField(this)(ref)

  def getCase[R <: T : ClassTag](implicit upc: UnionPropertyCodec[T]): Opt[Property[R]] =
    SubProperty.getCase[T, R](this)

  @silent
  def getSeq[E](implicit ev: T =:= Seq[E]): Seq[Property[E]] =
    SubProperty.getSeq(this.asInstanceOf[Property[Seq[E]]])

  def listenStructure[E](listener: SeqPatch[E] => Unit)(o: PropertyObservers)(implicit ev: Property[T] =:= Property[Seq[E]]): Unit = {
    o.observe(ev.apply(this), new PropertyObserver[Seq[E]] {
      override def propertyChanged(property: Property[Seq[E]]): Unit = {}

      override def propertyRemoved(property: Property[Seq[E]]): Unit = {}

      override def seqChanged(patch: SeqPatch[_]): Unit = {
        listener(patch.asInstanceOf[SeqPatch[E]])
      }
    })
  }

  def listen(listener: T => Unit, init: Boolean = false)(o: PropertyObservers)(implicit codec: PropertyCodec[T]): Unit = {
    o.observe(this, new PropertyObserver[T] {
      override def propertyChanged(property: Property[T]): Unit = {
        listener(property.get)
      }

      override def propertyRemoved(property: Property[T]): Unit = {}

      override def seqChanged(patch: SeqPatch[_]): Unit = {}
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

  class SeqMapProperty[K, V, T[_]](val value: SeqMap[K, Property[V]]) extends Property[T[V]]

  type SeqProperty[E] = SeqMapProperty[Int, E, Seq]

  def print(property: Property[_]): String = {
    property match {
      case s: SimpleProperty[_] => s.value.toString
      case r: RecordProperty[_] => "(" + r.fields.map { case (name, value) => s"$name -> ${print(value)}" }.mkString(", ") + ")"
      case u: UnionProperty[_] => s"${u.caseName}: ${print(u.value)}"
      case s: SeqMapProperty[_, _, _] => "[" + s.value.map(p => s"${p._1} -> ${print(p._2)}").mkString(", ") + "]"
    }
  }

}
