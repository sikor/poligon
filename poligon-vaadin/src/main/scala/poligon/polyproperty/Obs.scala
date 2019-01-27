package poligon.polyproperty

import poligon.polyproperty.PropertyObserver.PropertyObservers

trait Obs[T] {
  def listen(listener: T => Unit, init: Boolean)(implicit obs: PropertyObservers): Unit

  def map[R](f: T => R): Obs[R] = new MapObs[T, R](this, f)
}


//TODO: Merge this two cases into one with default identity map function
class PropertyObs[T](property: PropertyWithParent[T], implicit val codec: PropertyCodec[T]) extends Obs[T] {
  def listen(listener: T => Unit, init: Boolean)(implicit obs: PropertyObservers): Unit = {
    property.listen(listener, init)
  }
}

class MapObs[S, T](source: Obs[S], map: S => T) extends Obs[T] {
  def listen(listener: T => Unit, init: Boolean)(implicit obs: PropertyObservers): Unit = {
    source.listen(v => listener(map(v)), init)(obs)
  }
}