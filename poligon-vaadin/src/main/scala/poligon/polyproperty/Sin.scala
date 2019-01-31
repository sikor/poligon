package poligon.polyproperty

import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty.Sin.MapSin

trait Sin[T] {
  def rMap[R](f: R => T): Sin[R] = new MapSin[T, R](this, v => f(v))


  def set(v: T)(implicit po: PropertyObservers): Unit
}

object Sin {

  def apply[T: PropertyCodec](property: PropertyWithParent[T]) = new PropertySin[T](property)

  def apply[T](sub: Sin[T]*): Sin[T] = new Sin[T] {
    def set(v: T)(implicit po: PropertyObservers): Unit = sub.foreach(_.set(v))
  }

  class MapSin[S, T](child: Sin[S], map: T => S) extends Sin[T] {
    def set(v: T)(implicit po: PropertyObservers): Unit = child.set(map(v))
  }

  class PropertySin[T: PropertyCodec](property: PropertyWithParent[T]) extends Sin[T] {
    def set(v: T)(implicit po: PropertyObservers): Unit = property.set(v)
  }

}
