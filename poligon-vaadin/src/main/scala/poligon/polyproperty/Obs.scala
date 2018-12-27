package poligon.polyproperty

import poligon.polyproperty.PropertyObserver.PropertyObservers

trait Obs[T] {
  def listen(listener: T => Unit, init: Boolean)(obs: PropertyObservers): Unit

  def map[R](f: T => R): Obs[R] = new MapObs[T, R](this, f)
}

class PropertyObs[T](property: Property[T], codec: PropertyCodec[T]) extends Obs[T] {
  def listen(listener: T => Unit, init: Boolean)(obs: PropertyObservers): Unit = {
    property.listen(listener, init)(obs)(codec)
  }
}

class MapObs[S, T](source: Obs[S], map: S => T) extends Obs[T] {
  def listen(listener: T => Unit, init: Boolean)(obs: PropertyObservers): Unit = {
    source.listen(v => listener(map(v)), init)(obs)
  }
}