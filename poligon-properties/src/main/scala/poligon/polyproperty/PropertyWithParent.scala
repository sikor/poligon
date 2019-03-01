package poligon
package polyproperty

import monix.eval.Task
import poligon.polyproperty.Act.Act
import poligon.polyproperty.Obs.Obs
import poligon.polyproperty.Property.SortedMapProperty
import poligon.polyproperty.PropertyCodec.PropertyChange.{Added, EntryPatch}
import poligon.polyproperty.PropertyCodec.StructuralPropertyCodec
import poligon.polyproperty.PropertyCodec.StructuralPropertyCodec.StructuralChange
import poligon.polyproperty.PropertyObserver.PropertyObservers

import scala.collection.SortedMap

class PropertyWithParent[S](val property: Property[S], val parent: Opt[PropertyWithParent[_]],
                            private val refresher: () => Opt[S] = () => Opt.Empty) {

  override def toString = s"PropertyWithParent($property, $parent)"
}


object PropertyWithParent {
  def apply[T: PropertyCodec](value: () => T): PropertyWithParent[T] =
    new PropertyWithParent[T](PropertyCodec.newProperty[T](value()), Opt.Empty, () => value().opt)

  def apply[T: PropertyCodec](value: T): PropertyWithParent[T] =
    new PropertyWithParent[T](PropertyCodec.newProperty[T](value), Opt.Empty)

  implicit class GeneralPropertyExt[T](p: PropertyWithParent[T])(implicit c: PropertyCodec[T]) {
    def read: T = c.readProperty(p.property.asInstanceOf[c.PropertyType])

    def refresh: Act[Unit] = Act.defer(p.refresher().map(d => set(d)).getOrElse(Act.unit))

    def set(value: T): Act[Unit] = Act.create(implicit r => PropertyChanger.set(p, value))

    def setEnforcingListeners(value: T): Act[Unit] =
      Act.create(implicit r => PropertyChanger.set(p, value, enforceListeners = true))

    def listen(listener: T => Task[Unit], init: Boolean = false)(implicit o: PropertyObservers): Task[Unit] = {
      o.observe(p.property, new PropertyObserver[T] {
        override def propertyChanged(property: Property[T]): Task[Unit] = {
          listener(read)
        }

        override def propertyRemoved(property: Property[T]): Task[Unit] = {
          Task.unit
        }

        override def structureChange(patch: StructuralChange[_, _, T]): Task[Unit] = {
          Task.unit
        }
      })
      if (init) {
        listener(read)
      } else {
        Task.unit
      }
    }

    def obs: Obs[T] = Obs(p)

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

  implicit class SeqPropertyExt[T](val p: PropertyWithParent[Seq[T]])(implicit val c: SeqPropertyCodec[T]) {

    def getSeq: Seq[PropertyWithParent[T]] =
      SubProperty.getSeq(p.property).map(e => new PropertyWithParent[T](e, p.opt))

    def insert(index: Int, values: Seq[T]): Act[Unit] =
      Act.create(implicit po => PropertyChanger.insert(p, index, values: _*))

    def append(value: T*): Act[Unit] =
      Act.create(implicit po => PropertyChanger.append[T](p, value: _*))

    def remove(index: Int, count: Int): Act[Unit] =
      Act.create(implicit po => PropertyChanger.remove[T](p, index, count))

    def structObs: Obs[Struct[T]] = Obs.struct(p)
  }

  implicit class SortedMapPropertyExt[K, V](val p: PropertyWithParent[SortedMap[K, V]])(implicit val c: SortedMapPropertyCodec[K, V]) {

    def get(key: K): Opt[PropertyWithParent[V]] = {
      seqSortedMap.get(key).map(wrap)
    }

    def apply(key: K): PropertyWithParent[V] = {
      wrap(seqSortedMap(key))
    }

    def put(key: K, value: V): Act[Unit] =
      Act.create(implicit po => PropertyChanger.put(key, value, p, c))

    def remove(key: K): Act[Unit] =
      Act.create(implicit po => PropertyChanger.remove(key, p, c))

    private def wrap(s: Property[V]): PropertyWithParent[V] = {
      new PropertyWithParent[V](s, p.opt)
    }

    private def seqSortedMap: SeqSortedMap[K, Property[V]] = {
      p.property.asInstanceOf[SortedMapProperty[K, V, BSortedMap[K, V]]].value
    }

    def structObs: Obs[Struct[V]] = Obs.struct(p)
  }

  class StructuralChangeWithParents[K, V, T] private[PropertyWithParent](val property: PropertyWithParent[T],
                                                                         val modifications: EntryPatch[K, PropertyWithParent[V]])

  type Struct[T] = StructuralChangeWithParents[_, T, _]

  def listenStructure[K, V, T](p: PropertyWithParent[T], init: Boolean = false)
                              (listener: StructuralChangeWithParents[K, V, T] => Task[Unit])
                              (implicit
                               o: PropertyObservers,
                               c: StructuralPropertyCodec[K, V, T]): Task[Unit] = {
    o.observe(p.property, new PropertyObserver[T] {
      override def propertyChanged(property: Property[T]): Task[Unit] = {
        Task.unit
      }

      override def propertyRemoved(property: Property[T]): Task[Unit] = {
        Task.unit
      }

      override def structureChange(patch: StructuralChange[_, _, T]): Task[Unit] = {
        val sc = patch.asInstanceOf[StructuralChange[K, V, T]]
        listener(new StructuralChangeWithParents[K, V, T](p, sc.modifications.map(_.map(sp => new PropertyWithParent[V](sp, p.opt)))))
      }
    })

    if (init) {
      val entries = c.getEntries(p.property.asInstanceOf[c.PropertyType])
        .map(e => Added(e.map(sp => new PropertyWithParent[V](sp, p.opt))))
      listener(new StructuralChangeWithParents(p, entries))
    } else {
      Task.unit
    }
  }

}