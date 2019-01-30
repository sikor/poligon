package poligon.exampleapp.view

import poligon.exampleapp.services.DmService
import poligon.exampleapp.services.DmService.{DmTree, Node, Value}
import poligon.exampleapp.view.MainView.MainViewContentPresenter.ObjectsPanelContent
import poligon.exampleapp.view.ObjectsPanelPresenter.ActionStatus.Success
import poligon.exampleapp.view.ObjectsPanelPresenter._
import poligon.polyproperty.Property.Diff
import poligon.polyproperty.Property.Diff.{NoOp, Val}
import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty._

import scala.collection.SortedMap


object ObjectsPanelPresenter {

  sealed trait ActionStatus

  object ActionStatus {

    case object Draft extends ActionStatus

    case object Pending extends ActionStatus

    case object Success extends ActionStatus

    case object Failed extends ActionStatus

  }

  case class Action(status: ActionStatus, description: String)

  object Action extends HasSimplePropertyCodec[Action]

  sealed trait Resource {
    def name: String
  }

  case class ResourceInstance(idx: Int, value: String, formValue: Diff[String] = NoOp, lastAction: Diff[Action] = NoOp)

  object ResourceInstance extends HasRecordPropertyCodec[ResourceInstance]

  case class SingleResource(name: String, value: String, lastAction: Diff[Action] = NoOp, formValue: Diff[String] = NoOp) extends Resource

  object SingleResource extends HasRecordPropertyCodec[SingleResource]

  case class MultiResource(name: String, value: SortedMap[Int, ResourceInstance]) extends Resource

  object MultiResource extends HasRecordPropertyCodec[MultiResource]

  object Resource extends HasUnionPropertyCodec[Resource]

  case class ObjectInstance(id: Int,
                            resources: SortedMap[String, Resource],
                            lastAction: Diff[Action] = NoOp)

  case class SomeObject(name: String, instances: SortedMap[Int, ObjectInstance], lastAction: Diff[Action] = NoOp)

  object ObjectInstance extends HasRecordPropertyCodec[ObjectInstance]

  object SomeObject extends HasRecordPropertyCodec[SomeObject]

  def dmToObjects(dm: Node): SortedMap[String, SomeObject] = {
    dm.children.collect { case (cn, cv: Node) => (cn, cv) }.mkSortedMap(kv => kv._1, kv => nodeToObject(kv._1, kv._2))
  }

  def nodeToObject(name: String, node: Node): SomeObject = {
    SomeObject(name, node.children.collect { case (cn, cv: Node) => (cn, cv) }
      .mkSortedMap(kv => kv._1.toInt, kv => nodeToInstance(kv._1, kv._2)))
  }

  def nodeToInstance(name: String, node: Node): ObjectInstance = {
    ObjectInstance(name.toInt, node.children.mkSortedMap(kv => kv._1, kv => treeToResource(kv._1, kv._2)))
  }

  def treeToResource(name: String, node: DmTree): Resource = {
    node match {
      case n: Node =>
        MultiResource(name, n.children.collect { case (idx, Value(value)) => ResourceInstance(idx.toInt, value) }
          .mkSortedMap(r => r.idx, identity))
      case Value(value) =>
        SingleResource(name, value)
    }
  }

}

class ObjectsPanelPresenter(val dmService: DmService) extends ObjectsPanelContent {
  val model: PropertyWithParent[SortedMap[String, SomeObject]] = PropertyWithParent(() => dmToObjects(dmService.getDm))
  val newObjectName = PropertyWithParent("")

  def addObject(o: String)(implicit po: PropertyObservers): Unit = {
    model.put(o, SomeObject(o, SortedMap.empty))
  }

  def addInstance(o: String, i: Int)(implicit po: PropertyObservers): Unit = {
    val objectModel = findObject(o)
    objectModel.getField(_.instances).put(i, ObjectInstance(i, SortedMap.empty))
    objectModel.getField(_.lastAction).set(Val(Action(Success, s"instance added: $i")))
  }

  def addResource(o: String, i: Int, r: String, value: String)(implicit po: PropertyObservers): Unit = {
    findObjectInstance(o, i)
      .getField(_.resources).put(r, SingleResource(r, value))
  }

  def removeObject(o: String)(implicit po: PropertyObservers): Unit = {
    model.remove(o)
  }

  private def findObjectInstance(o: String, instance: Int) = {
    findObject(o).getField(_.instances)(instance)
  }

  private def findObject(o: String) = {
    model(o)
  }
}
