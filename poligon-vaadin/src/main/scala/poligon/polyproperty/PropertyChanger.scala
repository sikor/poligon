package poligon.polyproperty

import poligon.polyproperty.PropertyMarker.MarkedProperties
import poligon.polyproperty.PropertyObserver.ObservedProperties

/**
  * TODO:
  * 1. Add possibility to amend list without overwriting all values (method for add(index), remove(index)) + batching updates
  * 2. Use polyproperty instead of io.udash in example vaadin project
  * 3. Consider defining special property for Presenters -> Property that holds something that holds another property (flatMap?)
  *
  * Problem: cleaning bindings, avoiding memory leaks:
  * - Map from property to listeners: allow to hold references on view side as normally
  * - Removing listeners from map when property is removed + marking property as removed:
  *   allow to avoid memory leaks of map holding components properties that are no longer connected with any parent.
  * - Parent of view keeps map of listeners for each child and manage them together with child reference:
  *   allow to avoid keeping references to ui components that do not longer are displayed (parent ui components is the only who keeps the references)
  * Problem: sub presenters:
  * - When we change sub presenter, all views connected with it must be removed, and we must mark all its properties as removed (together with subpresenters)
  * - presenter should be referenced only from parents, and view hierarchies originating from one view.
  * - special property is not needed - mechanism with keeping listeners in parent view should be enough.
  **/
object PropertyChanger {
  def set[T: PropertyCodec](property: PropertyWithParent[T], value: T)(implicit observed: ObservedProperties): Unit = {
    val mp = new MarkedProperties()
    PropertyCodec.updateProperty(value, property.property, mp)
    PropertyMarker.traverseWithParents(property, mp.onPropertyChanged)
    mp.clearRemoved(observed.propertyRemoved)
    mp.clearChanged(observed.propertyChanged)
  }
}
