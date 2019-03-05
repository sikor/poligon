package poligon.polyproperty

import monix.eval.Task
import poligon.polyproperty.PropertyCodec.StructuralPropertyCodec
import poligon.polyproperty.PropertyObserver.{AnyPropertyObservers, GPropertyObservers}
import poligon.polyproperty.PropertyWithParent.{Struct, listenStructure}


object Obs {

  type BObs[+T, -D] = GObs[T, GPropertyObservers[D]]
  type AnyObs[+T] = BObs[T, Any]

  def apply[T: PropertyCodec](property: PropertyWithParent[T]): AnyObs[T] = new PropertyObs[T](property)

  def struct[K, V, T](property: PropertyWithParent[T])(implicit codec: StructuralPropertyCodec[K, V, T]): AnyObs[Struct[V]] =
    new StructObs[K, V, T](property)

  private class PropertyObs[T: PropertyCodec](property: PropertyWithParent[T]) extends AnyObs[T] {
    def listen(listener: T => Task[Unit], scope: GPropertyObservers[Any]): Task[Unit] = {
      property.listen(listener, init = true)(scope)
    }
  }

  private class StructObs[K, V, T](source: PropertyWithParent[T])(implicit codec: StructuralPropertyCodec[K, V, T]) extends AnyObs[Struct[V]] {
    def listen(listener: Struct[V] => Task[Unit], scope: AnyPropertyObservers): Task[Unit] =
      listenStructure[K, V, T](source, init = true)(v => listener(v))(scope, codec)
  }


}

