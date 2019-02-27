package poligon

import monix.eval.Task
import monix.reactive.Observable
import poligon.Extensions._
import poligon.comp.Comp
import poligon.comp.CompFamily.LayoutModification
import poligon.polyproperty.PropertyCodec.PropertyChange.{Added, Removed}
import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty.PropertyWithParent.Struct
import poligon.polyproperty.{Obs, PropertyWithParent}
import poligon.tran.TranslationKey.TranslationKeyOps

import scala.collection.immutable.{SortedMap, TreeMap}

trait Extensions {

  implicit def traversableOnceExtensions[C[X] <: TraversableOnce[X], A](coll: C[A]): TraversableOnceExtensions[C, A] =
    new TraversableOnceExtensions(coll)

  implicit def observableExtensions[T](observable: Observable[T]): ObservableExtensions[T] =
    new ObservableExtensions[T](observable)

  implicit def structObsExtensions[V](obs: Obs[Struct[V]]): StructObsExtensions[V] =
    new StructObsExtensions[V](obs)

  implicit def translationOps(s: String): TranslationKeyOps =
    new TranslationKeyOps(s)

  implicit def futureExtensions[T](f: Future[T]): FutureExtensions[T] =
    new FutureExtensions[T](f)

  implicit def taskExtensions[T](t: Task[T]): TaskExtensions[T] =
    new TaskExtensions[T](t)
}

object Extensions {

  class TraversableOnceExtensions[C[X] <: TraversableOnce[X], A](private val coll: C[A]) extends AnyVal {
    def mkSortedMap[K: Ordering, V](keyFun: A => K, valueFun: A => V): SortedMap[K, V] = {
      val res = TreeMap.newBuilder[K, V]
      coll.foreach { a =>
        res += ((keyFun(a), valueFun(a)))
      }
      res.result()
    }
  }

  private class ObservableObs[T](source: Observable[T]) extends Obs[T] {
    def listen(listener: T => Task[Unit])(implicit po: PropertyObservers): Task[Unit] = {
      po.subscribeToObservable(source, listener)
      Task.unit
    }
  }

  class ObservableExtensions[T](private val observable: Observable[T]) extends AnyVal {
    def toObs: Obs[T] = new ObservableObs[T](observable)
  }

  class StructObsExtensions[V](private val obs: Obs[Struct[V]]) extends AnyVal {
    def toLayoutMod(f: PropertyWithParent[V] => Comp): Obs[Seq[LayoutModification[Comp]]] =
      obs.map(s => s.modifications.map {
        case Added(entry) => LayoutModification.Added(entry.index, f(entry.value))
        case Removed(entry) => LayoutModification.Removed[Comp](entry.index)
      })
  }

  //TODO: this is risky because future can hold reference for long time (memory leak).
  // We relay on Future provider here that there is some timeout. One improvement would be to
  // hold reference only to RootPropertyObserver so leak would occur only when Promise holder lives longer
  // than whole view.
  private class FutureObs[T](source: Future[T]) extends Obs[Try[T]] {
    def listen(listener: Try[T] => Task[Unit])(implicit po: PropertyObservers): Task[Unit] = {
      Task.fromFuture(source).materialize.flatMap(listener)
    }
  }

  class FutureExtensions[T](private val future: Future[T]) extends AnyVal {
    def toObs: Obs[Try[T]] = new FutureObs[T](future)
  }

  private class TaskObs[T](source: Task[T]) extends Obs[Try[T]] {
    def listen(listener: Try[T] => Task[Unit])(implicit po: PropertyObservers): Task[Unit] = {
      source.materialize.flatMap(listener)
    }
  }

  class TaskExtensions[T](private val task: Task[T]) extends AnyVal {
    def toObs: Obs[Try[T]] = new TaskObs[T](task)
  }

}