package poligon
package polyproperty


import poligon.polyproperty.PropertyCodec.PropertyChange._

import scala.collection.mutable.ArrayBuffer

class SeqMap[K, V] {

  private var seq: ArrayBuffer[(K, V)] = new ArrayBuffer[(K, V)]()

  def map[V2](f: ((K, V)) => V2): ArrayBuffer[V2] = seq.map(f)

  def foreach(f: ((K, V)) => Unit): Unit = seq.foreach(f)

  def clear(): EntryPatch[K, V] = {
    val removed = seq.iterator.zipWithIndex.map { case ((k, v), i) => Removed(Entry(i, k, v)) }.toVector.reverse
    seq.clear()
    removed
  }

  def size(): Int = seq.size

  def slice(from: Int, until: Int): ArrayBuffer[(K, V)] = seq.slice(from, until)

  def values(): Seq[V] = seq.map(_._2)

  def entries(): Seq[Entry[K, V]] = seq.iterator.zipWithIndex.map { case ((k, v), i) => Entry(i, k, v) }.toSeq

  /**
    * @return indices removed before insertion because of duplicates
    */
  def insert(index: Int, values: (K, V)*): EntryPatch[K, V] = {
    val newKeys = values.iterator.map(_._1).toSet
    val duplicatesReversed = findDuplicates(newKeys).reverse
    val removed = duplicatesReversed.map { i =>
      val (k, v) = seq.remove(i)
      Removed(Entry(i, k, v))
    }
    val beforeInsertIndexCount = duplicatesReversed.count(_ < index)
    val newIndex = index - beforeInsertIndexCount
    seq.insert(newIndex, values: _*)
    val inserted = values.iterator.zipWithIndex.map {
      case ((k, v), i) => Added(Entry(i, k, v))
    }.toSeq
    removed ++ inserted
  }

  /**
    * @return indices removed before insertion because of duplicates
    */
  def appendAll(values: Seq[(K, V)]): EntryPatch[K, V] = {
    insert(seq.size, values: _*)
  }

  /**
    * @return indices removed before insertion because of duplicates
    */
  def append(key: K, value: V): EntryPatch[K, V] = {
    appendAll(Seq((key, value)))
  }

  def remove(index: Int, count: Int): EntryPatch[K, V] = {
    val removed = slice(index, index + count)
    seq.remove(index, count)
    removed.zipWithIndex.map { case ((k, v), i) =>
      Removed(Entry(index + i, k, v))
    }
  }

  def get(key: K): Opt[V] = {
    seq.findOpt(v => v._1 == key).map(_._2)
  }

  /**
    *
    * set value under given key or adds the new entry at the end if key does not exist
    */
  def set(key: K, value: V): Int = {
    val index = findIndex(key)
    index match {
      case Opt(i) => seq.update(i, (key, value))
      case Opt.Empty => appendAll(Seq((key, value)))
    }
    index.getOrElse(seq.size)
  }

  def remove(key: K): EntryPatch[K, V] = {
    findIndex(key).map { i =>
      val v = seq.remove(i)._2
      Seq(Removed(Entry(i, key, v)))
    }.getOrElse(Seq.empty)
  }

  def retainKeys(toRetain: BSet[K]): EntryPatch[K, V] = {
    val removed = seq.iterator.zipWithIndex.collect {
      case ((k, v), i) if !toRetain.contains(k) =>
        Removed(Entry(i, k, v))
    }.toSeq
    seq = seq.filter(e => toRetain.contains(e._1))
    removed.reverse
  }

  def update(keys: Iterable[K], updateValue: (K, V) => Unit, insertValue: K => V): EntryPatch[K, V] = {
    val patches = Vector.newBuilder[Modification[K, V]]
    for ((k, i) <- keys.zipWithIndex) {
      findIndex(k) match {
        case Opt(currentIndex) =>
          if (i != currentIndex) {
            //currentIndex must be greater than i
            patches ++= remove(i, currentIndex - i)
          }
          updateValue(k, seq(i)._2)
        case Opt.Empty =>
          patches ++= insert(i, (k, insertValue(k)))
      }
    }
    patches.result()
  }

  private def findIndex(key: K) = {
    seq.indexWhere(v => v._1 == key).opt.filter(_ != -1)
  }

  private def findDuplicates(values: BSet[K]): Seq[Int] = {
    val duplicates = new ArrayBuffer[Int]
    for (i <- seq.indices) {
      if (values.contains(seq(i)._1)) {
        duplicates += i
      }
    }
    duplicates
  }
}

object SeqMap {


}