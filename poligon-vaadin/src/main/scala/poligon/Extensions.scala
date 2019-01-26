package poligon

import poligon.Extensions.TraversableOnceExtensions

import scala.collection.immutable.{SortedMap, TreeMap}

trait Extensions {

  implicit def traversableOnceExtensions[C[X] <: TraversableOnce[X], A](coll: C[A]): TraversableOnceExtensions[C, A] =
    new TraversableOnceExtensions(coll)
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

}