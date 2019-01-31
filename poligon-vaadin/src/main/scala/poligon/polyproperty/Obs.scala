package poligon.polyproperty

import poligon.polyproperty.Obs.Obs
import poligon.polyproperty.PropertyObserver.PropertyObservers

object Obs {
  type Obs[T] = MapObs[_, T]

  def apply[T: PropertyCodec](property: PropertyWithParent[T]): MapObs[T, T] = new MapObs[T, T](property, identity)
}

class MapObs[S: PropertyCodec, T](property: PropertyWithParent[S], map: S => T) {
  def listen(listener: T => Unit, init: Boolean)(implicit obs: PropertyObservers): Unit = {
    property.listen(v => listener(map(v)), init)(obs)
  }

  def map[R](f: T => R): Obs[R] = new MapObs[S, R](property, v => f(map(v)))

}