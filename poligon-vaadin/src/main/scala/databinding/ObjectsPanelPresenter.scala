package databinding

import databinding.MainView.MainViewContentPresenter.ObjectsPanelContent
import databinding.ObjectsPanelPresenter.ActionStatus.Success
import databinding.ObjectsPanelPresenter._
import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty._


/**
  * Why own properties:
  * - Using GenCodes nice for rest RPC
  * - Unable to use sealed hierarchy in Udash Properties
  * - Adding HProperty functionality
  */
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


}

class ObjectsPanelPresenter extends ObjectsPanelContent {
  private val model: PropertyWithParent[Seq[SomeObject]] = {
    PropertyWithParent(
      Seq(
        SomeObject("object 1", Vector(
          ObjectInstance(3, Vector(
            SingleResource("resource 1", "value1"),
            MultiResource("multi resource", Seq(ResourceInstance(2, "value 2"), ResourceInstance(3, "value 3")))))))))
  }

  def getModel: Property[Seq[SomeObject]] = model.property

  def setSingleResourceValue(o: String, instance: Int, resource: String, value: String)(implicit po: PropertyObservers): Unit = {
    val resourceModel = findResource(o, instance, resource)
    resourceModel.getCase[SingleResource].get.getSubProperty(_.ref(_.formValue)).set(Some(value))
  }

  def setMultiResourceValue(o: String, instance: Int, resource: String, resourcesInstance: Int, value: String)(implicit po: PropertyObservers): Unit = {
    val resourceModel = findResource(o, instance, resource)
    resourceModel
      .getCase[MultiResource].get
      .getSubProperty(_.ref(_.value)).getSeq[ResourceInstance]
      .find(i => i.get.idx == resourcesInstance).get
      .getSubProperty(_.ref(_.formValue))
      .set(Some(value))
  }

  def saveResources(): Unit = {

  }

  def addObject(o: String)(implicit po: PropertyObservers): Unit = {
    model.append(SomeObject(o, Seq.empty))
  }

  def addInstance(o: String, i: Int)(implicit po: PropertyObservers): Unit = {
    val objectModel = findObject(o)
    objectModel.getSubProperty(_.ref(_.instances)).append(ObjectInstance(i, Seq.empty))
    objectModel.getSubProperty(_.ref(_.lastAction)).set(Some(Action(Success, s"instance added: $i")))
  }

  def addResource(o: String, i: Int, r: String, value: String)(implicit po: PropertyObservers): Unit = {
    findObjectInstance(o, i)
      .getSubProperty(_.ref(_.resources)).append[Resource](SingleResource(r, value))
  }

  def removeObject(o: String)(implicit po: PropertyObservers): Unit = {
    val idx = model.get.indexWhere(_.name == o)
    model.remove[SomeObject](idx, 1)
  }

  private def findResource(o: String, instance: Int, resource: String) = {
    findObjectInstance(o, instance)
      .getSubProperty(_.ref(_.resources)).getSeq[Resource].find(p => p.get.name == resource).get
  }

  private def findObjectInstance(o: String, instance: Int) = {
    findObject(o)
      .getSubProperty(_.ref(_.instances)).getSeq[ObjectInstance].find(p => p.get.id == instance).get
  }

  private def findObject(o: String) = {
    model.getSeq[SomeObject].find(p => p.get.name == o).get
  }
}
