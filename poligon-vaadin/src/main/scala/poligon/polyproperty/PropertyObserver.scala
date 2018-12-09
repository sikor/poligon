package poligon
package polyproperty

import poligon.polyproperty.Property.SeqProperty
import poligon.polyproperty.PropertyObserver.SeqPatch

import scala.collection.mutable

trait PropertyObserver[T] {
  def propertyChanged(property: Property[T]): Unit

  def propertyRemoved(property: Property[T]): Unit

  def seqChanged(patch: SeqPatch[_]): Unit
}

object PropertyObserver {
  type AnyPropertyObserver = PropertyObserver[Any]
  type ObserversMapT = mutable.HashMap[Property[_], mutable.Set[AnyPropertyObserver]]
  type ObserversMultiMap = mutable.MultiMap[Property[_], AnyPropertyObserver]
  type OMM = ObserversMapT with ObserversMultiMap

  class SeqPatch[E](val property: SeqProperty[E], val idx: Int, val added: Seq[Property[E]], val removed: Seq[Property[E]])

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

    def seqChanged(patch: SeqPatch[_]): Unit = {
      observers.get(patch.property).foreach(_.foreach(l => l.seqChanged(patch)))
    }

    private[PropertyObserver] def clear(): Unit = {
      observers.clear()
    }
  }

  class PropertyObservers(rootOpt: Opt[PropertyObservers]) {
    private val root = rootOpt.getOrElse(this)

    private val map = new ObserversMap()
    private val subObservers: mutable.HashSet[PropertyObservers] = new mutable.HashSet()

    def createSubObservers(): PropertyObservers = {
      val r = new PropertyObservers(root.opt)
      subObservers += r
      r
    }

    def removeSubObservers(po: PropertyObservers): Unit = {
      subObservers.remove(po)
      po.clearAllData()
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

    def seqChanged(patch: SeqPatch[_]): Unit = {
      traverseAll(_.seqChanged(patch))
    }

    private def traverseAll(onObserversMap: ObserversMap => Unit): Unit = {
      def visit(po: PropertyObservers): Unit = {
        onObserversMap(po.map)
        po.subObservers.foreach(visit)
      }

      visit(root)
    }

    private def clearAllData(): Unit = {
      map.clear()
      subObservers.foreach(_.clearAllData())
      subObservers.clear()
    }
  }


}