package poligon
package polyproperty

import poligon.polyproperty.Property.SortedMapProperty
import poligon.polyproperty.PropertyCodec.PropertyChange.{Added, EntryPatch}
import poligon.polyproperty.PropertyCodec.StructuralPropertyCodec
import poligon.polyproperty.PropertyCodec.StructuralPropertyCodec.StructuralChange
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

        override def structureChange(patch: StructuralChange[_, _, T]): Unit = {}
      })
      if (init) {
        listener(get)
      }
    }

    def obs: Obs[T] = new PropertyObs[T](p.property, c)

    def map[R](f: T => R): Obs[R] = obs.map(f)
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

  implicit class SeqPropertyExt[T](val p: PropertyWithParent[Seq[T]])(implicit val c: SeqPropertyCodec[T])
    extends StructuralPropertyExt[Int, T, Seq[T]] {

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

  implicit class SortedMapPropertyExt[K, V](val p: PropertyWithParent[SortedMap[K, V]])(implicit val c: SortedMapPropertyCodec[K, V])
    extends StructuralPropertyExt[K, V, SortedMap[K, V]] {

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

  class StructuralChangeWithParents[K, V, T] private[PropertyWithParent](val property: PropertyWithParent[T],
                                                                         val modifications: EntryPatch[K, PropertyWithParent[V]])

  trait StructuralPropertyExt[K, V, T] {
    def p: PropertyWithParent[T]

    def c: StructuralPropertyCodec[K, V, T]

    def listenStructure(listener: StructuralChangeWithParents[K, V, T] => Unit)(implicit o: PropertyObservers): Unit = {
      o.observe(p.property, new PropertyObserver[T] {
        override def propertyChanged(property: Property[T]): Unit = {}

        override def propertyRemoved(property: Property[T]): Unit = {}

        override def structureChange(patch: StructuralChange[_, _, T]): Unit = {
          val sc = patch.asInstanceOf[StructuralChange[K, V, T]]
          listener(new StructuralChangeWithParents[K, V, T](p, sc.modifications.map(_.map(sp => new PropertyWithParent[V](sp, p.opt)))))
        }
      })
    }
  }

  def listenStructure[K, V, T](p: PropertyWithParent[T], init: Boolean = false)
                              (listener: StructuralChangeWithParents[K, V, T] => Unit)
                              (implicit
                               o: PropertyObservers,
                               c: StructuralPropertyCodec[K, V, T]): Unit = {
    o.observe(p.property, new PropertyObserver[T] {
      override def propertyChanged(property: Property[T]): Unit = {}

      override def propertyRemoved(property: Property[T]): Unit = {}

      override def structureChange(patch: StructuralChange[_, _, T]): Unit = {
        val sc = patch.asInstanceOf[StructuralChange[K, V, T]]
        listener(new StructuralChangeWithParents[K, V, T](p, sc.modifications.map(_.map(sp => new PropertyWithParent[V](sp, p.opt)))))
      }
    })

    if (init) {
      val entries = c.getEntries(p.property.asInstanceOf[c.PropertyType])
        .map(e => Added(e.map(sp => new PropertyWithParent[V](sp, p.opt))))
      listener(new StructuralChangeWithParents(p, entries))
    }
  }

}