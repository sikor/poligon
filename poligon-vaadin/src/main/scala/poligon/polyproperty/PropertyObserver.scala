package poligon.polyproperty

import scala.collection.mutable

trait PropertyObserver[P[_] <: Property[_], T] {
  def propertyChanged(property: P[T]): Unit

  def propertyRemoved(property: P[T]): Unit
}

object PropertyObserver {
  type AnyPropertyObserver = PropertyObserver[Property, Any]

  class ObservedProperties(private val observers: mutable.HashMap[Property[_], mutable.Set[AnyPropertyObserver]]
    with mutable.MultiMap[Property[_], AnyPropertyObserver]) extends AnyVal {
    def observe[P[_] <: Property[_], T](property: P[T], propertyObserver: PropertyObserver[P, T]): Unit = {
      observers.addBinding(property.asInstanceOf[Property[_]], propertyObserver.asInstanceOf[AnyPropertyObserver])
    }

    def propertyChanged(property: Property[_]): Unit = {
      observers.getOpt(property).foreach(_.foreach(l => l.propertyChanged(property.asInstanceOf[Property[Any]])))
    }

    def propertyRemoved(property: Property[_]): Unit = {
      observers.getOpt(property).foreach(_.foreach(l => l.propertyRemoved(property.asInstanceOf[Property[Any]])))
    }
  }


}