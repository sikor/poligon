package poligon.exampleapp.view

import poligon.exampleapp.services.DmService.{DmTree, Node, Value}
import poligon.polyproperty.Property.Diff
import poligon.polyproperty.Property.Diff.NoOp
import poligon.polyproperty._

import scala.collection.SortedMap


object ObjectPanelModel {

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
                            lastAction: Diff[Action] = NoOp,
                            newResourceName: Diff[String] = NoOp,
                            newResourceValue: Diff[String] = NoOp)

  case class SomeObject(name: String, instances: SortedMap[Int, ObjectInstance], lastAction: Diff[Action] = NoOp, newInstanceNumber: Diff[Int] = NoOp)

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
