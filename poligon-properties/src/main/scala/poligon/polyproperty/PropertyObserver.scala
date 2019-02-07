package poligon.polyproperty

import poligon.polyproperty.PropertyCodec.StructuralPropertyCodec.StructuralChange

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait PropertyObserver[T] {
  def propertyChanged(property: Property[T]): Unit

  def propertyRemoved(property: Property[T]): Unit

  def structureChange(patch: StructuralChange[_, _, T]): Unit
}

object PropertyObserver {
  type AnyPropertyObserver = PropertyObserver[Any]
  type ObserversMapT = mutable.HashMap[Property[_], mutable.Set[AnyPropertyObserver]]
  type ObserversMultiMap = mutable.MultiMap[Property[_], AnyPropertyObserver]
  type OMM = ObserversMapT with ObserversMultiMap

  class ObserversMap(private val observers: OMM = new ObserversMapT with ObserversMultiMap) extends AnyVal {

    def observe[P[_] <: Property[_], T](property: P[T], propertyObserver: PropertyObserver[T]): Unit = {
      //perhaps mark property as removed after removal and throw exception if property arg is already removed
      observers.addBinding(property.asInstanceOf[Property[_]], propertyObserver.asInstanceOf[AnyPropertyObserver])
    }

    def propertyChanged(property: Property[_]): Unit = {
      observers.getOpt(property).foreach(_.foreach(l => l.propertyChanged(property.asInstanceOf[Property[Any]])))
    }

    def propertyRemoved(property: Property[_]): Unit = {
      observers.remove(property).foreach(_.foreach(l => l.propertyRemoved(property.asInstanceOf[Property[Any]])))
    }

    def structureChange(patch: StructuralChange[_, _, _]): Unit = {
      observers.get(patch.property).foreach(_.foreach(l => l.structureChange(patch.asInstanceOf[StructuralChange[_, _, Any]])))
    }

    private[PropertyObserver] def clear(): Unit = {
      observers.clear()
    }
  }

  def createRoot: PropertyObservers = {
    val root = new RootPropertyObservers
    val po = new PropertyObservers(root)
    root.setPropertyObservers(po)
    po
  }

  class RootPropertyObservers private[PropertyObserver]() {

    private var po: PropertyObservers = _

    private[PropertyObserver] def setPropertyObservers(p: PropertyObservers): Unit = po = p

    def propertyChanged(property: Property[_]): Unit = {
      traverseAll(_.propertyChanged(property))
    }

    def propertyRemoved(property: Property[_]): Unit = {
      traverseAll(_.propertyRemoved(property))
    }

    def structureChange(patch: StructuralChange[_, _, _]): Unit = {
      traverseAll(_.structureChange(patch))
    }

    private def traverseAll(onObserversMap: ObserversMap => Unit): Unit = {
      def visit(po: PropertyObservers): Unit = {
        onObserversMap(po.map)
        po.subObservers.values.foreach(visit)
      }

      visit(po)
    }
  }

  object RootPropertyObservers {
    implicit def fromPo(implicit po: PropertyObservers): RootPropertyObservers = po.root
  }

  class PropertyObservers private[PropertyObserver](val root: RootPropertyObservers) {

    private[PropertyObserver] val map = new ObserversMap()
    private[PropertyObserver] val subObservers: mutable.HashMap[Any, PropertyObservers] = new mutable.HashMap()

    private val cancellableResources = new ArrayBuffer[() => Unit]

    def createSubObservers(): PropertyObservers = new PropertyObservers(root)

    def registerSubObservers(key: Any, po: PropertyObservers): Unit = {
      require(!subObservers.contains(key))
      subObservers.put(key, po)
    }

    def deregisterSubObservers(key: Any): Unit = {
      subObservers.remove(key).foreach(_.cancellableResources.foreach(c => c()))
    }

    def observe[T](property: Property[T], propertyObserver: PropertyObserver[T]): Unit = {
      map.observe(property, propertyObserver)
    }

    def resourceOpened(cancellable: () => Unit): Unit = {
      cancellableResources.append(cancellable)
    }
  }


}