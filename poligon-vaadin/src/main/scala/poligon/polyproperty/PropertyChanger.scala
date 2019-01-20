package poligon.polyproperty

import poligon.polyproperty.PropertyObserver.PropertyObservers

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
    val allChanges = if (childrenChanges.nonEmpty) {
      childrenChanges ++ PropertyMarker.parentsChanged(property)
    } else {
      childrenChanges
    }
    allChanges.foreach { change =>

    }
    mp.clearRemoved(po.propertyRemoved)
    mp.clearChanged(po.propertyChanged)
  }

  def insert[E: SeqPropertyCodec](
                                   property: PropertyWithParent[Seq[E]],
                                   index: Int,
                                   value: E*)(
                                   implicit
                                   observed: PropertyObservers): Unit = {
    val seqProp = SubProperty.asSeqProperty(property.property)
    val patch = SeqPropertyCodec[E].insert(seqProp, index, value)
    observed.seqChanged(patch)
  }

  def append[E: SeqPropertyCodec](
                                   property: PropertyWithParent[Seq[E]],
                                   value: E*)(
                                   implicit
                                   observed: PropertyObservers): Unit = {
    val seqProp = SubProperty.asSeqProperty(property.property)
    val patch = SeqPropertyCodec[E].append(seqProp, value)
    observed.seqChanged(patch)
  }

  def remove[E: SeqPropertyCodec](
                                   property: PropertyWithParent[Seq[E]],
                                   index: Int,
                                   count: Int)(
                                   implicit
                                   observed: PropertyObservers): Unit = {
    val seqProp = SubProperty.asSeqProperty(property.property)
    val patch = SeqPropertyCodec[E].remove(seqProp, index, count)
    patch.removed.foreach(observed.propertyRemoved)
    observed.seqChanged(patch)
  }


}
