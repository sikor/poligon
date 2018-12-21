package poligon.polyproperty

import poligon.polyproperty.PropertyMarker.MarkedProperties
import poligon.polyproperty.PropertyObserver.PropertyObservers

/**
  * TODO:
  * 1. Add possibility to amend list without overwriting all values (method for add(index), remove(index)) + batching updates
  * 2. Allow to create listeners registry for subview - it should allow to remove easily all listeners from it
  * 3. Optional: Allow marking properties as removed to disallow listening on removed property
  * 4. Use polyproperty instead of io.udash in example vaadin project
  *
  * Problem: cleaning bindings, avoiding memory leaks:
  * - Map from property to listeners: allow to hold references on view side as normally
  * - Removing listeners from map when property is removed + marking property as removed:
  * allow to avoid memory leaks of map holding components properties that are no longer connected with any parent.
  * - Parent of view keeps map of listeners for each child and manage them together with child reference:
  * allow to avoid keeping references to ui components that do not longer are displayed (parent ui components is the only who keeps the references)
  * Problem: sub presenters:
  * - When we change sub presenter, all views connected with it must be removed, and we must mark all its properties as removed (together with subpresenters)
  * - presenter should be referenced only from parents, and view hierarchies originating from one view.
  * - some mechanism to remove presenter properties from listeners map
  * in case sublisteners map is not created for all usages of presenter
  **/
object PropertyChanger {
  def set[T: PropertyCodec](property: PropertyWithParent[T], value: T)(implicit observed: PropertyObservers): Unit = {
    val mp = new MarkedProperties()
    PropertyCodec.updateProperty(value, property.property, mp)
    PropertyMarker.traverseWithParents(property, mp.onPropertyChanged)
    mp.clearRemoved(observed.propertyRemoved)
    mp.clearChanged(observed.propertyChanged)
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
