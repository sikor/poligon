package poligon.polyproperty

import poligon.polyproperty.PropertyMarker.MarkedProperties
import poligon.polyproperty.PropertyObserver.ObservedProperties

/**
  * TODO:
  * 1. Add possibility to amend list without overwriting all values
  * 2. Use polyproperty instead of io.udash in example vaadin project
  * 3. Consider defining special property for Presenters -> Property that holds something that holds another property (flatMap?)
  */
object PropertyChanger {
  def set[T: PropertyCodec](property: PropertyWithParent[T], value: T)(implicit observed: ObservedProperties): Unit = {
    val mp = new MarkedProperties()
    PropertyCodec.updateProperty(value, property.property, mp)
    PropertyMarker.traverseWithParents(property, mp.onPropertyChanged)
    mp.clearRemoved(observed.propertyRemoved)
    mp.clearChanged(observed.propertyChanged)
  }
}
