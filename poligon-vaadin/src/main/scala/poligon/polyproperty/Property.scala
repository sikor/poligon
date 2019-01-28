package poligon
package polyproperty

import poligon.polyproperty.Property.Diff.{NoOp, Val}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait Property[T]

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

  sealed trait Diff[+T] {
    def toOpt: Opt[T] = this match {
      case NoOp => Opt.Empty
      case Val(s) => s.opt
    }
  }

  object Diff {

    case object NoOp extends Diff[Nothing]

    case class Val[T](value: T) extends Diff[T]

  }

}
