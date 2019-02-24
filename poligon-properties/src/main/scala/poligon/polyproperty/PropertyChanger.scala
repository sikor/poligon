package poligon
package polyproperty

import monix.eval.Task
import poligon.polyproperty.Property.SortedMapProperty
import poligon.polyproperty.PropertyCodec.PropertyChange
import poligon.polyproperty.PropertyCodec.PropertyChange.{Removed, ValueChange}
import poligon.polyproperty.PropertyCodec.StructuralPropertyCodec.StructuralChange
import poligon.polyproperty.PropertyObserver.RootPropertyObservers

import scala.collection.SortedMap

/**
  *
  * Types of changes:
  * - Value change (concerns all properties for example: RecordProperty, SimpleProperty)
  * - Structure change (concerns only: MapProperty, UnionProperty). This also means value change but in this case some more serious
  * changes might be needed - changing components count or type.
  *
  * If component does not use directly the value of the property but only propagates sub-properties to sub-components then,
  * It might be interested only in the structure change.
  **/
object PropertyChanger {
  def set[T: PropertyCodec](property: PropertyWithParent[T], value: T, enforceListeners: Boolean = false)
                           (implicit po: RootPropertyObservers): Task[Unit] = {
    val childrenChanges = PropertyCodec.updateProperty(value, property.property)
    val changes = withParents(property, childrenChanges)
    if (enforceListeners && changes.isEmpty) {
      callListeners(Seq(new ValueChange(property.property)), po)
    } else {
      callListeners(changes, po)
    }
  }

  def insert[E: SeqPropertyCodec](
                                   property: PropertyWithParent[Seq[E]],
                                   index: Int,
                                   value: E*)(
                                   implicit
                                   observed: RootPropertyObservers): Task[Unit] = {
    val seqProp = SubProperty.asSeqProperty(property.property)
    val patch = SeqPropertyCodec[E].insert(seqProp, index, value)
    callListeners(withParents(property, patch), observed)
  }

  def append[E: SeqPropertyCodec](
                                   property: PropertyWithParent[Seq[E]],
                                   value: E*)(
                                   implicit
                                   observed: RootPropertyObservers): Task[Unit] = {
    val seqProp = SubProperty.asSeqProperty(property.property)
    val patch = SeqPropertyCodec[E].append(seqProp, value)
    callListeners(withParents(property, patch), observed)
  }

  def remove[E: SeqPropertyCodec](
                                   property: PropertyWithParent[Seq[E]],
                                   index: Int,
                                   count: Int)(
                                   implicit
                                   observed: RootPropertyObservers): Task[Unit] = {
    val seqProp = SubProperty.asSeqProperty(property.property)
    val patch = SeqPropertyCodec[E].remove(seqProp, index, count)
    callListeners(withParents(property, patch), observed)
  }

  def put[K, V](
                 key: K,
                 value: V,
                 property: PropertyWithParent[SortedMap[K, V]],
                 codec: SortedMapPropertyCodec[K, V])(
                 implicit
                 observed: RootPropertyObservers): Task[Unit] = {
    val mapProp = property.property.asInstanceOf[SortedMapProperty[K, V, BSortedMap[K, V]]]
    val patch = codec.put(key, value, mapProp)
    callListeners(withParents(property, patch), observed)
  }


  def remove[K, V](
                    key: K,
                    property: PropertyWithParent[SortedMap[K, V]],
                    codec: SortedMapPropertyCodec[K, V])(
                    implicit
                    observed: RootPropertyObservers): Task[Unit] = {
    val mapProp = property.property.asInstanceOf[SortedMapProperty[K, V, BSortedMap[K, V]]]
    val patch = codec.remove(key, mapProp)
    callListeners(withParents(property, patch), observed)
  }

  private def withParents(property: PropertyWithParent[_], childrenChanges: Seq[PropertyChange]) = {
    if (childrenChanges.nonEmpty) {
      childrenChanges ++ PropertyMarker.parentsChanged(property)
    } else {
      childrenChanges
    }
  }

  private def callListeners(changes: Seq[PropertyChange], po: RootPropertyObservers): Task[Unit] = {
    val tasks = changes.flatMap {
      case v: ValueChange =>
        Vector(po.propertyChanged(v.property))
      case sp: StructuralChange[_, _, _] =>
        val sc = po.structureChange(sp)
        val removed = sp.modifications.map {
          case Removed(entry) =>
            po.propertyRemoved(entry.value)
          case _ =>
            Task.unit
        }.toVector
        sc +: removed
    }
    Task.gatherUnordered(tasks).map(_ => ())
  }
}
