package poligon
package polyproperty

import poligon.polyproperty.Property.PropertyChange.{SeqMapStructuralChange, SeqStructuralChange, UnionChange}

import scala.collection.mutable

trait PropertyObserver[T] {
  def propertyChanged(property: Property[T]): Unit

  def propertyRemoved(property: Property[T]): Unit

  def seqChanged(patch: SeqStructuralChange[_]): Unit

  def seqMapChanged(patch: SeqMapStructuralChange[_, _, _]): Unit

  def unionChanged(patch: UnionChange[T]): Unit
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

    def seqChanged(patch: SeqStructuralChange[_]): Unit = {
      observers.get(patch.property).foreach(_.foreach(l => l.seqChanged(patch)))
    }

    def seqMapChanged(patch: SeqMapStructuralChange[_, _, _]): Unit = {
      observers.get(patch.property).foreach(_.foreach(l => l.seqMapChanged(patch)))
    }

    def unionChanged(patch: UnionChange[_]): Unit = {
      observers.get(patch.property).foreach(_.foreach(l => l.unionChanged(patch.asInstanceOf[UnionChange[Any]])))
    }

    private[PropertyObserver] def clear(): Unit = {
      observers.clear()
    }
  }

  class PropertyObservers(private val parent: Opt[PropertyObservers]) {
    private val root: PropertyObservers = parent.map(_.root).getOrElse(this)

    private val map = new ObserversMap()
    private val subObservers: mutable.HashMap[AnyRef, PropertyObservers] = new mutable.HashMap()

    def createSubObservers(): PropertyObservers = {
      new PropertyObservers(this.opt)
    }

    def registerSubObservers(key: AnyRef, po: PropertyObservers): Unit = {
      require(!subObservers.contains(key))
      subObservers.put(key, po)
    }

    def deregisterSubObservers(key: AnyRef): Unit = {
      subObservers.remove(key)
    }

    def observe[T](property: Property[T], propertyObserver: PropertyObserver[T]): Unit = {
      map.observe(property, propertyObserver)
    }

    def propertyChanged(property: Property[_]): Unit = {
      traverseAll(_.propertyChanged(property))
    }

    def propertyRemoved(property: Property[_]): Unit = {
      traverseAll(_.propertyRemoved(property))
    }

    def seqChanged(patch: SeqStructuralChange[_]): Unit = {
      traverseAll(_.seqChanged(patch))
    }

    def seqMapChanged(patch: SeqMapStructuralChange[_, _, _]): Unit = {
      traverseAll(_.seqMapChanged(patch))
    }

    def unionChanged(patch: UnionChange[_]): Unit = {
      traverseAll(_.unionChanged(patch))
    }

    private def traverseAll(onObserversMap: ObserversMap => Unit): Unit = {
      def visit(po: PropertyObservers): Unit = {
        onObserversMap(po.map)
        po.subObservers.values.foreach(visit)
      }

      visit(root)
    }
  }


}