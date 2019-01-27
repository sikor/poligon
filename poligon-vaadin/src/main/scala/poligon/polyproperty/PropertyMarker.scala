package poligon
package polyproperty

import poligon.polyproperty.Property._
import poligon.polyproperty.PropertyCodec.PropertyChange
import poligon.polyproperty.PropertyCodec.PropertyChange.ValueChange

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

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

  def traverseWithParents(p: PropertyWithParent[_], onProperty: Property[_] => Unit): Unit = {
    onProperty(p.property)
    traverseParents(p, onProperty)
  }

  def traverseWithChildren(p: Property[_], onProperty: Property[_] => Unit): Unit = {
    onProperty(p)
    traverseChildren(p, onProperty)
  }

  def traverseChildren(p: Property[_], onChild: Property[_] => Unit): Unit = {
    p match {
      case _: SimpleProperty[_] =>
      case u: UnionProperty[_] =>
        onChild(u.value)
        traverseChildren(u.value, onChild)
      case r: RecordProperty[_] =>
        r.fields.valuesIterator.foreach { f =>
          onChild(f)
          traverseChildren(f, onChild)
        }
      case s: SeqProperty[_] =>
        s.value.foreach { e =>
          onChild(e)
          traverseChildren(e, onChild)
        }
      case s: SeqMapProperty[_, _, _] =>
        s.value.foreach { e =>
          onChild(e._2)
          traverseChildren(e._2, onChild)
        }
      case s: SortedMapProperty[_, _, _] =>
        s.value.foreach { e =>
          onChild(e._2)
          traverseChildren(e._2, onChild)
        }
    }
  }

  @tailrec
  def traverseParents(p: PropertyWithParent[_], onParent: Property[_] => Unit): Unit = {
    p.parent match {
      case Opt.Empty =>
      case Opt(parent) =>
        onParent(parent.property)
        traverseParents(parent, onParent)
    }
  }

  def parentsChanged(p: PropertyWithParent[_]): Seq[PropertyChange] = {
    val results = new ArrayBuffer[PropertyChange]
    traverseParents(p, parent => results += new ValueChange(parent))
    results
  }

}
