package databinding

import com.avsystem.commons.misc.Opt
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

    case object Pending extends ActionStatus

    case object Success extends ActionStatus

    case object Failed extends ActionStatus

  }

  case class Action(status: ActionStatus, description: String)

  object Action extends HasSimplePropertyCodec[Action]

  sealed trait Resource {
    def name: String
  }

  case class SingleResource(name: String, value: String, lastAction: Opt[Action] = Opt.Empty) extends Resource

  case class MultiResource(name: String, value: Map[Int, String], lastAction: Opt[Action] = Opt.Empty) extends Resource

  object Resource extends HasUnionPropertyCodec[Resource]

  case class ObjectInstance(id: Int, resources: Seq[Resource], lastAction: Opt[Action] = Opt.Empty)

  case class SomeObject(name: String, instances: Seq[ObjectInstance], lastAction: Opt[Action] = Opt.Empty)

  object ObjectInstance extends HasRecordPropertyCodec[ObjectInstance]

  object SomeObject extends HasRecordPropertyCodec[SomeObject]


}

class ObjectsPanelPresenter extends Presenter {
  private val model: PropertyWithParent[Seq[SomeObject]] = {
    PropertyWithParent(
      Seq(
        SomeObject("object 1", Vector(
          ObjectInstance(3, Vector(
            SingleResource("resource 1", "value1"),
            MultiResource("multi resource", Map(2 -> "value 2", 3 -> "value 3"))))))))
  }

  def getModel: Property[Seq[SomeObject]] = model.property

  def setResourceValue(o: String, instance: Int, resource: String, resourceInstance: Option[Int], value: String)(implicit po: PropertyObservers): Unit = {
    val resourceModel = findResource(o, instance, resource)

    val newVal = resourceModel.get match {
      case s: SingleResource => SingleResource(resource, value, Opt(Action(Success, s"value set: $value")))
      case m: MultiResource =>
        val newMap = m.value + (resourceInstance.get -> value)
        MultiResource(resource, newMap, Opt(Action(Success, s"value set: ${resourceInstance.get} -> $value")))
    }

    resourceModel.set(newVal)
  }

  def addObject(o: String)(implicit po: PropertyObservers): Unit = {
    model.append(SomeObject(o, Seq.empty))
  }

  def addInstance(o: String, i: Int)(implicit po: PropertyObservers): Unit = {
    val objectModel = findObject(o)
    objectModel.getSubProperty(_.ref(_.instances)).append(ObjectInstance(i, Seq.empty))
    objectModel.getSubProperty(_.ref(_.lastAction)).set(Opt(Action(Success, s"instance added: $i")))
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
