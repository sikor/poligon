package poligon

import monix.execution.Scheduler
import monix.reactive.Observable
import poligon.Extensions.{ObservableExtensions, TraversableOnceExtensions}
import poligon.polyproperty.Obs
import poligon.polyproperty.PropertyObserver.PropertyObservers

import scala.collection.immutable.{SortedMap, TreeMap}

trait Extensions {

  implicit def traversableOnceExtensions[C[X] <: TraversableOnce[X], A](coll: C[A]): TraversableOnceExtensions[C, A] =
    new TraversableOnceExtensions(coll)

  implicit def observableExtensions[T](observable: Observable[T]): ObservableExtensions[T] =
    new ObservableExtensions[T](observable)
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

  private class ObservableObs[T](source: Observable[T])(implicit s: Scheduler) extends Obs[T] {
    def listen(listener: T => Unit)(implicit obs: PropertyObservers): Unit = {
      val cancelable = source.foreach(listener)
      obs.resourceOpened(() => cancelable.cancel())
    }
  }

  class ObservableExtensions[T](private val observable: Observable[T]) extends AnyVal {
    def toObs(implicit s: Scheduler): Obs[T] = new ObservableObs[T](observable)
  }

}