package poligon
package polyproperty

import poligon.polyproperty.Property.PropertyChange.SeqMapStructuralChange
import poligon.polyproperty.Property.SortedMapProperty
import poligon.polyproperty.PropertyObserver.PropertyObservers

import scala.collection.SortedMap

class PropertyWithParent[S](val property: Property[S], val parent: Opt[PropertyWithParent[_]])


object PropertyWithParent {
  def apply[T: PropertyCodec](value: T): PropertyWithParent[T] =
    new PropertyWithParent[T](PropertyCodec.newProperty[T](value), Opt.Empty)

  implicit class GeneralPropertyExt[T](p: PropertyWithParent[T])(implicit c: PropertyCodec[T]) {
    def get: T = p.property.get

    def set(value: T)(implicit observed: PropertyObservers): Unit = {
      PropertyChanger.set(p, value)
    }

    def listen(listener: T => Unit, init: Boolean = false)(implicit o: PropertyObservers): Unit = {
      o.observe(p.property, new PropertyObserver[T] {
        override def propertyChanged(property: Property[T]): Unit = {
          listener(property.get)
        }

        override def propertyRemoved(property: Property[T]): Unit = {}

        override def structureChange(patch: SeqMapStructuralChange[_, _, T]): Unit = {}
      })
      if (init) {
        listener(get)
      }
    }
  }

  implicit class UnionPropertyExt[S](p: PropertyWithParent[S])(implicit c: UnionPropertyCodec[S]) {
    def getCase[T <: S : ClassTag]: Opt[PropertyWithParent[T]] =
      SubProperty.getCase[S, T](p.property).map(caseProperty => new PropertyWithParent[T](caseProperty, p.opt))
  }

  implicit class RecordPropertyExt[T](p: PropertyWithParent[T])(implicit c: RecordPropertyCodec[T]) {

    def getField[S](f: T => S): PropertyWithParent[S] = macro poligon.PropertyMacros.getField[S]

    def internalGetField[S](name: String): PropertyWithParent[S] = {
      new PropertyWithParent[S](SubProperty.getField(p.property, name), p.opt)
    }
  }

  implicit class SeqPropertyExt[T](p: PropertyWithParent[Seq[T]])(implicit c: SeqPropertyCodec[T]) {
    def getSeq: Seq[PropertyWithParent[T]] =
      SubProperty.getSeq(p.property).map(e => new PropertyWithParent[T](e, p.opt))

    def insert(index: Int, value: T*)(implicit observed: PropertyObservers): Unit = {
      PropertyChanger.insert[T](p, index, value: _*)
    }

    def append(value: T*)(implicit observed: PropertyObservers): Unit = {
      PropertyChanger.append[T](p, value: _*)
    }

    def remove(index: Int, count: Int)(implicit observed: PropertyObservers): Unit = {
      PropertyChanger.remove[T](p, index, count)
    }
  }

  implicit class SortedMapExt[K, V](p: PropertyWithParent[SortedMap[K, V]])(implicit c: SortedMapPropertyCodec[K, V]) {
    def get(key: K): Opt[PropertyWithParent[V]] = {
      seqSortedMap.get(key).map(wrap)
    }

    def apply(key: K): PropertyWithParent[V] = {
      wrap(seqSortedMap(key))
    }

    def put(key: K, value: V)(implicit observed: PropertyObservers): Unit = {
      PropertyChanger.put(key, value, p, c)
    }

    def remove(key: K)(implicit observed: PropertyObservers): Unit = {
      PropertyChanger.remove(key, p, c)
    }

    private def wrap(s: Property[V]): PropertyWithParent[V] = {
      new PropertyWithParent[V](s, p.opt)
    }

    private def seqSortedMap: SeqSortedMap[K, Property[V]] = {
      p.property.asInstanceOf[SortedMapProperty[K, V, BSortedMap[K, V]]].value
    }
  }

  implicit class StructuralPropertyExt[C[_] <: Iterable[_], E](p: PropertyWithParent[C[E]])(implicit c: PropertyCodec[C[E]]) {

  }

}