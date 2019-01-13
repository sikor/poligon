package poligon
package polyproperty


import scala.collection.mutable.ArrayBuffer

class SeqMap[K, V] {

  private val seq: ArrayBuffer[(K, V)] = new ArrayBuffer[(K, V)]()

  def map[V2](f: ((K, V)) => V2): ArrayBuffer[V2] = seq.map(f)

  def foreach(f: ((K, V)) => Unit): Unit = seq.foreach(f)

  def clear(): Unit = seq.clear()

  def size(): Int = seq.size

  def slice(from: Int, until: Int): ArrayBuffer[(K, V)] = seq.slice(from, until)

  /**
    * @return indices removed before insertion because of duplicates
    */
  def insert(index: Int, values: Seq[(K, V)]): Seq[Int] = {
    val newKeys = values.iterator.map(_._1).toSet
    val duplicates = findDuplicates(newKeys)
    val beforeInsertIndexCount = duplicates.count(_ < index)
    val newIndex = index - beforeInsertIndexCount
    duplicates.foreach(seq.remove)
    seq.insert(newIndex, values: _*)
    duplicates
  }

  /**
    * @return indices removed before insertion because of duplicates
    */
  def append(values: Seq[(K, V)]): Seq[Int] = {
    insert(seq.size, values)
  }

  /**
    * @return indices removed before insertion because of duplicates
    */
  def append(key: K, value: V): Seq[Int] = {
    append(Seq((key, value)))
  }

  def remove(index: Int, count: Int): Unit = {
    seq.remove(index, count)
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
      case Opt.Empty => append(Seq((key, value)))
    }
    index.getOrElse(seq.size)
  }

  def remove(key: K): Opt[V] = {
    val index = findIndex(key)
    index.map(seq.remove).map(_._2)
  }

  private def findIndex(key: K) = {
    seq.indexWhere(v => v._1 == key).opt.filter(_ != -1)
  }

  private def findDuplicates(values: Set[K]): Seq[Int] = {
    val duplicates = new ArrayBuffer[Int]
    for (i <- seq.indices) {
      if (values.contains(seq(i)._1)) {
        duplicates += i
      }
    }
    duplicates
  }
}
