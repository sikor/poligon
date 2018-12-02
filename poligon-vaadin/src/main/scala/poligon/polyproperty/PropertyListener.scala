package poligon.polyproperty

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
object PropertyListener {

}
