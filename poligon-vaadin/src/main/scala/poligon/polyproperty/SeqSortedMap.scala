package poligon
package polyproperty

import poligon.polyproperty.Property.PropertyChange.{Added, Modification, Removed}
import poligon.polyproperty.SeqMap.{Entry, EntryPatch}

import scala.collection.mutable

class SeqSortedMap[K, V](init: Iterable[(K, V)])(implicit ordering: Ordering[K]) {

  private val tree: mutable.TreeMap[K, V] = new mutable.TreeMap[K, V]()

  init.foreach { case (k, v) => tree.put(k, v) }

  def update(newKeys: Seq[K], updateValue: (K, V) => Unit, insertValue: K => V): EntryPatch[K, V] = {
    val newKeysSorted = newKeys.sorted
    val patches = Vector.newBuilder[Modification[Entry[K, V]]]
    val currentKeys = tree.keysIterator.buffered
    for ((k, i) <- newKeysSorted.zipWithIndex) {
      def insertUpdateOrMove(): Unit = {
        if (currentKeys.isEmpty || ordering.lt(k, currentKeys.head)) {
          patches += Added(Entry(i, k, insertValue(k)))
        } else if (currentKeys.head == k) {
          updateValue(k, tree(k))
          currentKeys.next()
        } else {
          patches += Removed(Entry(i, k, tree(k)))
          currentKeys.next()
          insertUpdateOrMove()
        }
      }

      insertUpdateOrMove()
    }
    currentKeys.foreach(k => patches += Removed(Entry(newKeys.size, k, tree(k))))

    patches.result().foreach {
      case Added(e) => tree.put(e.key, e.value)
      case Removed(e) => tree.remove(e.key)
    }

    patches.result()
  }

  def foreach(f: ((K, V)) => Unit): Unit = tree.foreach(f)

  def map[V2](f: ((K, V)) => V2): Iterable[V2] = tree.map(f)
}
