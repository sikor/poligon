package poligon.polyproperty

import poligon.BSortedMap
import poligon.polyproperty.Property.SortedMapProperty
import poligon.polyproperty.PropertyCodec.PropertyChange
import poligon.polyproperty.PropertyCodec.PropertyChange.{Removed, ValueChange}
import poligon.polyproperty.PropertyCodec.StructuralPropertyCodec.StructuralChange
import poligon.polyproperty.PropertyObserver.PropertyObservers

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
  def set[T: PropertyCodec](property: PropertyWithParent[T], value: T)(implicit po: PropertyObservers): Unit = {
    val childrenChanges = PropertyCodec.updateProperty(value, property.property)
    val changes = withParents(property, childrenChanges)
    callListeners(changes, po)
  }

  def insert[E: SeqPropertyCodec](
                                   property: PropertyWithParent[Seq[E]],
                                   index: Int,
                                   value: E*)(
                                   implicit
                                   observed: PropertyObservers): Unit = {
    val seqProp = SubProperty.asSeqProperty(property.property)
    val patch = SeqPropertyCodec[E].insert(seqProp, index, value)
    callListeners(withParents(property, patch), observed)
  }

  def append[E: SeqPropertyCodec](
                                   property: PropertyWithParent[Seq[E]],
                                   value: E*)(
                                   implicit
                                   observed: PropertyObservers): Unit = {
    val seqProp = SubProperty.asSeqProperty(property.property)
    val patch = SeqPropertyCodec[E].append(seqProp, value)
    callListeners(withParents(property, patch), observed)
  }

  def remove[E: SeqPropertyCodec](
                                   property: PropertyWithParent[Seq[E]],
                                   index: Int,
                                   count: Int)(
                                   implicit
                                   observed: PropertyObservers): Unit = {
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
                 observed: PropertyObservers): Unit = {
    val mapProp = property.property.asInstanceOf[SortedMapProperty[K, V, BSortedMap[K, V]]]
    val patch = codec.put(key, value, mapProp)
    callListeners(withParents(property, patch), observed)
  }


  def remove[K, V](
                    key: K,
                    property: PropertyWithParent[SortedMap[K, V]],
                    codec: SortedMapPropertyCodec[K, V])(
                    implicit
                    observed: PropertyObservers): Unit = {
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

  private def callListeners(changes: Seq[PropertyChange], po: PropertyObservers): Unit = {
    changes.foreach {
      case v: ValueChange =>
        po.propertyChanged(v.property)
      case sp: StructuralChange[_, _, _] =>
        po.structureChange(sp)
        sp.modifications.foreach {
          case Removed(entry) =>
            po.propertyRemoved(entry.value)
          case _ =>
        }
    }
  }
}
