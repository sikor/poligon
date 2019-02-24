package poligon.polyproperty

import monix.eval.Task
import monix.execution.Cancelable
import poligon.polyproperty.PropertyCodec.StructuralPropertyCodec.StructuralChange

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait PropertyObserver[T] {
  def propertyChanged(property: Property[T]): Task[Unit]

  def propertyRemoved(property: Property[T]): Task[Unit]

  def structureChange(patch: StructuralChange[_, _, T]): Task[Unit]
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

    def propertyChanged(property: Property[_]): Task[Unit] = {
      visitAll(property, l => l.propertyChanged(property.asInstanceOf[Property[Any]]))
    }

    def propertyRemoved(property: Property[_]): Task[Unit] = {
      visitAll(property, l => l.propertyRemoved(property.asInstanceOf[Property[Any]]))
    }

    def structureChange(patch: StructuralChange[_, _, _]): Task[Unit] = {
      visitAll(patch.property, l => l.structureChange(patch.asInstanceOf[StructuralChange[_, _, Any]]))
    }

    private def visitAll(property: Property[_], f: AnyPropertyObserver => Task[Unit]): Task[Unit] = {
      val tasks = observers.get(property).map(_.iterator.map(l => f(l)).toSeq).getOrElse(Seq.empty)
      Task.gatherUnordered(tasks).map(_ => ())
    }

    private[PropertyObserver] def clear(): Unit = {
      observers.clear()
    }
  }

  def createRoot(run: Task[Unit] => Cancelable): PropertyObservers = {
    val root = new RootPropertyObservers(run)
    val po = new PropertyObservers(root)
    root.setPropertyObservers(po)
    po
  }

  class RootPropertyObservers private[PropertyObserver](private val runF: Task[Unit] => Cancelable) {

    private var po: PropertyObservers = _

    private[PropertyObserver] def setPropertyObservers(p: PropertyObservers): Unit = po = p

    def run(task: Task[Unit]): Cancelable = runF(task)

    def propertyChanged(property: Property[_]): Task[Unit] = {
      traverseAll(_.propertyChanged(property))
    }

    def propertyRemoved(property: Property[_]): Task[Unit] = {
      traverseAll(_.propertyRemoved(property))
    }

    def structureChange(patch: StructuralChange[_, _, _]): Task[Unit] = {
      traverseAll(_.structureChange(patch))
    }

    private def traverseAll(onObserversMap: ObserversMap => Task[Unit]): Task[Unit] = {
      def visit(po: PropertyObservers): Task[Unit] = {
        val currentAction = onObserversMap(po.map)
        val childrenActions = po.subObservers.values.map(visit)
        Task.gatherUnordered(currentAction +: childrenActions.toSeq).map(_ => ())
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

    //TODO: remove cancelable when finished
    private val cancelables = new ArrayBuffer[Cancelable]

    def createSubObservers(): PropertyObservers = new PropertyObservers(root)

    def registerSubObservers(key: Any, po: PropertyObservers): Unit = {
      require(!subObservers.contains(key))
      subObservers.put(key, po)
    }

    def deregisterSubObservers(key: Any): Unit = {
      subObservers.remove(key).foreach(_.deregisterRecursively())
    }

    private def deregisterRecursively(): Unit = {
      cancelables.foreach(c => c.cancel())
      subObservers.values.foreach(_.deregisterRecursively())
    }

    def observe[T](property: Property[T], propertyObserver: PropertyObserver[T]): Unit = {
      map.observe(property, propertyObserver)
    }

    def runTask(task: Task[Unit]): Unit = {
      cancelables.append(root.run(task))
    }
  }


}