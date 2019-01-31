package poligon.polyproperty

import poligon.polyproperty.PropertyObserver.PropertyObservers

object Sin {

  type Sin[T] = MapSin[_, T]

  def apply[T: PropertyCodec](property: PropertyWithParent[T]) = new MapSin[T, T](property, identity)

  class MapSin[S: PropertyCodec, T](property: PropertyWithParent[S], map: T => S) {
    def rMap[R](f: R => T): Sin[R] = new MapSin[S, R](property, v => map(f(v)))

    def set(v: T)(implicit po: PropertyObservers): Unit = property.set(map(v))
  }

}
