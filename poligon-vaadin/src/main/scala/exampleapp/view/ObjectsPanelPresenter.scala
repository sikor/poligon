package exampleapp.view

import exampleapp.services.DmService
import exampleapp.services.DmService.{DmTree, Node, Value}
import exampleapp.view.MainView.MainViewContentPresenter.ObjectsPanelContent
import exampleapp.view.ObjectsPanelPresenter.ActionStatus.Success
import exampleapp.view.ObjectsPanelPresenter._
import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty._


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

  case class ResourceInstance(idx: Int, value: String, formValue: Option[String] = None)

  object ResourceInstance extends HasRecordPropertyCodec[ResourceInstance]

  case class SingleResource(name: String, value: String, lastAction: Option[Action] = None, formValue: Option[String] = None) extends Resource

  object SingleResource extends HasRecordPropertyCodec[SingleResource]

  case class MultiResource(name: String, value: Seq[ResourceInstance], lastAction: Option[Action] = None) extends Resource

  object MultiResource extends HasRecordPropertyCodec[MultiResource]

  object Resource extends HasUnionPropertyCodec[Resource]

  case class ObjectInstance(id: Int, resources: Seq[Resource], lastAction: Option[Action] = None)

  case class SomeObject(name: String, instances: Seq[ObjectInstance], lastAction: Option[Action] = None)

  object ObjectInstance extends HasRecordPropertyCodec[ObjectInstance]

  object SomeObject extends HasRecordPropertyCodec[SomeObject]

  def dmToObjects(dm: Node): Seq[SomeObject] = {
    dm.children.collect { case (cn, cv: Node) => nodeToObject(cn, cv) }.toSeq
  }

  def nodeToObject(name: String, node: Node): SomeObject = {
    SomeObject(name, node.children.collect { case (cn, cv: Node) => nodeToInstance(cn, cv) }.toSeq)
  }

  def nodeToInstance(name: String, node: Node): ObjectInstance = {
    ObjectInstance(name.toInt, node.children.map { case (cn, cv) => treeToResource(cn, cv) }.toSeq)
  }

  def treeToResource(name: String, node: DmTree): Resource = {
    node match {
      case n: Node =>
        MultiResource(name, n.children.collect { case (idx, Value(value)) => ResourceInstance(idx.toInt, value) }.toSeq)
      case Value(value) =>
        SingleResource(name, value)
    }
  }

}

class ObjectsPanelPresenter(dmService: DmService) extends ObjectsPanelContent {
  private val model: PropertyWithParent[Seq[SomeObject]] = PropertyWithParent(dmToObjects(dmService.getDm))


  def getModel: Property[Seq[SomeObject]] = model.property

  def setSingleResourceValue(o: String, instance: Int, resource: String, value: String)(implicit po: PropertyObservers): Unit = {
    val resourceModel = findResource(o, instance, resource)
    resourceModel.getCase[SingleResource].get.getField(_.formValue).set(Some(value))
  }

  def setMultiResourceValue(o: String, instance: Int, resource: String, resourcesInstance: Int, value: String)(implicit po: PropertyObservers): Unit = {
    val resourceModel = findResource(o, instance, resource)
    resourceModel
      .getCase[MultiResource].get
      .getField(_.value).getSeq
      .find(i => i.get.idx == resourcesInstance).get
      .getField(_.formValue)
      .set(Some(value))
  }

  def saveResources(): Unit = {

  }

  def addObject(o: String)(implicit po: PropertyObservers): Unit = {
    model.append(SomeObject(o, Seq.empty))
  }

  def addInstance(o: String, i: Int)(implicit po: PropertyObservers): Unit = {
    val objectModel = findObject(o)
    objectModel.getField(_.instances).append(ObjectInstance(i, Seq.empty))
    objectModel.getField(_.lastAction).set(Some(Action(Success, s"instance added: $i")))
  }

  def addResource(o: String, i: Int, r: String, value: String)(implicit po: PropertyObservers): Unit = {
    findObjectInstance(o, i)
      .getField(_.resources).append(SingleResource(r, value))
  }

  def removeObject(o: String)(implicit po: PropertyObservers): Unit = {
    val idx = model.get.indexWhere(_.name == o)
    model.remove(idx, 1)
  }

  private def findResource(o: String, instance: Int, resource: String) = {
    findObjectInstance(o, instance)
      .getField(_.resources).getSeq.find(p => p.get.name == resource).get
  }

  private def findObjectInstance(o: String, instance: Int) = {
    findObject(o)
      .getField(_.instances).getSeq.find(p => p.get.id == instance).get
  }

  private def findObject(o: String) = {
    model.getSeq.find(p => p.get.name == o).get
  }
}
