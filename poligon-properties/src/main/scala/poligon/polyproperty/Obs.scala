package poligon.polyproperty

import monix.eval.Task
import poligon.polyproperty.Obs.MapObs
import poligon.polyproperty.PropertyCodec.StructuralPropertyCodec
import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty.PropertyWithParent.{Struct, listenStructure}

trait Obs[+T] {
  self =>

  def listenAsync(listener: T => Task[Unit])(implicit obs: PropertyObservers): Unit

  def listen(listener: T => Unit)(implicit obs: PropertyObservers): Unit =
    listenAsync(v => Task.now(listener(v)))

  def map[R](f: T => R): Obs[R] = mapAsync(v => Task.now(f(v)))

  def mapAsync[R](f: T => Task[R]): Obs[R] = new MapObs[T, R](this, f)

  def flatMap[R](f: T => Obs[R]): Obs[R] = new Obs[R] {
    def listenAsync(listener: R => Task[Unit])(implicit obs: PropertyObservers): Unit = {
      self.listen { t =>
        f(t).listenAsync(listener)
      }
    }
  }
}

object Obs {

  def apply[T: PropertyCodec](property: PropertyWithParent[T]): Obs[T] = new PropertyObs[T](property)

  def constant[T](value: T): Obs[T] = new Obs[T] {
    def listenAsync(listener: T => Task[Unit])(implicit obs: PropertyObservers): Unit = listener(value)
  }

  def struct[K, V, T](property: PropertyWithParent[T])(implicit codec: StructuralPropertyCodec[K, V, T]): Obs[Struct[V]] =
    new StructObs[K, V, T](property)

  private class PropertyObs[T: PropertyCodec](property: PropertyWithParent[T]) extends Obs[T] {
    def listenAsync(listener: T => Task[Unit])(implicit obs: PropertyObservers): Unit = {
      property.listen(listener, init = true)
    }
  }

  private class MapObs[S, T](source: Obs[S], map: S => Task[T]) extends Obs[T] {
    def listenAsync(listener: T => Task[Unit])(implicit obs: PropertyObservers): Unit = {
      source.listenAsync(v => map(v).flatMap(fv => listener(fv)))(obs)
    }
  }

  private class StructObs[K, V, T](source: PropertyWithParent[T])(implicit codec: StructuralPropertyCodec[K, V, T]) extends Obs[Struct[V]] {
    def listenAsync(listener: Struct[V] => Task[Unit])(implicit po: PropertyObservers): Unit =
      listenStructure[K, V, T](source, init = true)(v => listener(v))
  }


}

