package poligon
package polyproperty

import poligon.polyproperty.Property.{RecordProperty, SeqProperty, SimpleProperty, UnionProperty}

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Requirements:
  * 1. One way relation: No reference from properties to views to avoid memory leak and deregistering
  * 2. Binding: When property is changed all views that use this property are notified
  *
  * Idea 1:
  * - Decoupled listeners: Listeners are kept in Map (property -> views), this map is kept on view side
  * - Eventing: Presenter after each update gives the list of properties that are updated, view must poll from the queue
  *
  * Idea 2:
  * - Decoupled listeners
  * - Dirty flag: presenter notifies that it is dirty, and traverse all properties to check which are updated and notifies views if needed.
  * - It requires additional flag on each property which is cleared during traversal/additional set of properties
  * - We can define wrapper for property root, it maintains the set of dirty properties.
  * - Create second class that will maintain the map for listeners, It will be view helper and will allow to create binding
  */
object PropertyMarker {

  class MarkedProperties(private val properties: mutable.HashSet[Property[_]] = new mutable.HashSet[Property[_]]()) extends AnyVal {
    def mark(p: Property[_]): Unit = properties += p

    def clear(onProperty: Property[_] => Unit): Unit = {
      properties.foreach(onProperty)
      properties.clear()
    }
  }

  def markProperty(p: PropertyWithParent[_])(implicit mp: MarkedProperties): Unit = {
    mp.mark(p.property)
    markParents(p)
    markChildren(p.property)
  }

  @tailrec
  private def markParents(p: PropertyWithParent[_])(implicit mp: MarkedProperties): Unit = {
    p.parent match {
      case Opt.Empty =>
      case Opt(parent) =>
        mp.mark(parent.property)
        markParents(parent)
    }
  }

  private def markChildren(p: Property[_])(implicit mp: MarkedProperties): Unit = {
    p match {
      case _: SimpleProperty[_] =>
      case u: UnionProperty[_] =>
        mp.mark(u.value)
        markChildren(u.value)
      case r: RecordProperty[_] =>
        r.fields.valuesIterator.foreach { f =>
          mp.mark(f)
          markChildren(f)
        }
      case s: SeqProperty[_] =>
        s.value.foreach { e =>
          mp.mark(e)
          markChildren(e)
        }
    }
  }

}
