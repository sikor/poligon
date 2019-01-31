package poligon.polyproperty

import poligon.polyproperty.Obs.MapObs
import poligon.polyproperty.PropertyCodec.StructuralPropertyCodec
import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty.PropertyWithParent.{Struct, listenStructure}

trait Obs[T] {
  def listen(listener: T => Unit)(implicit obs: PropertyObservers): Unit

  def map[R](f: T => R): Obs[R] = new MapObs[T, R](this, f)
}

object Obs {

  def apply[T: PropertyCodec](property: PropertyWithParent[T]): Obs[T] = new PropertyObs[T](property)

  def struct[K, V, T](property: PropertyWithParent[T])(implicit codec: StructuralPropertyCodec[K, V, T]): Obs[Struct[V]] =
    new StructObs[K, V, T](property)

  private class PropertyObs[T: PropertyCodec](property: PropertyWithParent[T]) extends Obs[T] {
    def listen(listener: T => Unit)(implicit obs: PropertyObservers): Unit = {
      property.listen(listener, init = true)
    }
  }

  private class MapObs[S, T](source: Obs[S], map: S => T) extends Obs[T] {
    def listen(listener: T => Unit)(implicit obs: PropertyObservers): Unit = {
      source.listen(v => listener(map(v)))(obs)
    }
  }

  private class StructObs[K, V, T](source: PropertyWithParent[T])(implicit codec: StructuralPropertyCodec[K, V, T]) extends Obs[Struct[V]] {
    def listen(listener: Struct[V] => Unit)(implicit po: PropertyObservers): Unit =
      listenStructure[K, V, T](source, init = true)(v => listener(v))
  }

}

