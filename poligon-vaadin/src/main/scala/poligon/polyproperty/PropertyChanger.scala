package poligon.polyproperty

import poligon.polyproperty.PropertyMarker.MarkedProperties
import poligon.polyproperty.PropertyObserver.ObservedProperties

object PropertyChanger {
  def set[T: PropertyCodec](property: PropertyWithParent[T], value: T)(implicit observed: ObservedProperties): Unit = {
    val mp = new MarkedProperties()
    PropertyCodec.updateProperty(value, property.property, mp)
    PropertyMarker.traverseWithParents(property, mp.onPropertyChanged)
    mp.clearRemoved(observed.propertyRemoved)
    mp.clearChanged(observed.propertyChanged)
  }
}
