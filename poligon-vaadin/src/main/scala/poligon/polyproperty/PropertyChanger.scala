package poligon.polyproperty

import poligon.polyproperty.PropertyMarker.MarkedProperties
import poligon.polyproperty.PropertyObserver.ObservedProperties

/**
  * TODO:
  * 1. updateProperty should take lambda to notify which children are updated because seq children are usuallay not updated
  * as well as new union case
  * 2. markedProperties should allow to mark only parents
  */
object PropertyChanger {
  def set[T: PropertyCodec](property: PropertyWithParent[T], value: T)(implicit observed: ObservedProperties): Unit = {
    PropertyCodec.updateProperty(value, property.property)
    val mp = new MarkedProperties()
    PropertyMarker.markProperty(property)(mp)
    mp.clear(observed.propertyChanged)
  }
}
