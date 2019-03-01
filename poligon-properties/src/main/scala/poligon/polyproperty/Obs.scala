package poligon.polyproperty

import monix.eval.Task
import poligon.polyproperty.PropertyCodec.StructuralPropertyCodec
import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty.PropertyWithParent.{Struct, listenStructure}


object Obs {

  type Obs[T] = GObs[T, PropertyObservers]

  def apply[T: PropertyCodec](property: PropertyWithParent[T]): Obs[T] = new PropertyObs[T](property)


  def struct[K, V, T](property: PropertyWithParent[T])(implicit codec: StructuralPropertyCodec[K, V, T]): Obs[Struct[V]] =
    new StructObs[K, V, T](property)

  private class PropertyObs[T: PropertyCodec](property: PropertyWithParent[T]) extends Obs[T] {
    def listen(listener: T => Task[Unit], scope: PropertyObservers): Task[Unit] = {
      property.listen(listener, init = true)(scope)
    }
  }

  private class StructObs[K, V, T](source: PropertyWithParent[T])(implicit codec: StructuralPropertyCodec[K, V, T]) extends Obs[Struct[V]] {
    def listen(listener: Struct[V] => Task[Unit], scope: PropertyObservers): Task[Unit] =
      listenStructure[K, V, T](source, init = true)(v => listener(v))(scope, codec)
  }


}

