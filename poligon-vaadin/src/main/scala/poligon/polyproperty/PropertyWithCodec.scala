package poligon.polyproperty

import com.avsystem.commons.misc.Opt
import poligon.polyproperty.Property.{RecordProperty, SeqProperty, SimpleProperty, UnionProperty}
import poligon.polyproperty.PropertyWithCodec.PropertyListener

import scala.collection.mutable.ArrayBuffer

class PropertyWithCodec[T](
                            private val property: Property[T],
                            val codec: PropertyCodec[T],
                            private val parentProperty: Opt[PropertyWithCodec[_]]) {

  private val listeners: ArrayBuffer[PropertyListener[T]] = new ArrayBuffer[PropertyListener[T]]()
  private var listenersInFire: Boolean = false

  def addListener(listener: PropertyListener[T]): Unit = {
    requireListenersNotInFire("Cannot add new listener")
    listeners += listener
  }

  private def requireListenersNotInFire(reason: String): Unit = {
    if (listenersInFire || parentProperty.exists(_.listenersInFire)) {
      throw new IllegalStateException(s"Property listeners executing in progress. $reason")
    }
  }

  /**
    * In case of update from parent we don't have to fire listeners because parent is responsible for that
    */
  def updateFromParent(newValue: T): Unit = {
    codec.updateProperty(newValue, property.asInstanceOf[codec.PropertyType])
  }

  def update(newValue: T): Unit = {
    requireListenersNotInFire("Cannot update value")
    codec.updateProperty(newValue, property.asInstanceOf[codec.PropertyType])
    fireValueChangeListeners()
  }

  private def fireValueChangeListeners(): Unit = {
    listenersInFire = true
    listeners.foreach(_.onValueChange(getValue))
    parentProperty.foreach(_.fireValueChangeListeners())
    listenersInFire = false
  }

  def getValue: T = codec.readProperty(property.asInstanceOf[codec.PropertyType])
}

object PropertyWithCodec {

  trait PropertyListener[T] {
    def onValueChange(newValue: T): Unit
  }

  def print(property: Property[_]): String = {
    property match {
      case s: SimpleProperty[_] => s.value.toString
      case r: RecordProperty[_] => "(" + r.fields.map { case (name, value) => s"$name -> ${print(value.property)}" }.mkString(", ") + ")"
      case u: UnionProperty[_] => s"${u.caseName}: ${print(u.value.property)}"
      case s: SeqProperty[_] => "[" + s.value.map(p => print(p.property)).mkString(", ") + "]"
    }
  }
}



