package databinding

import com.avsystem.commons.misc.Opt
import databinding.ObjectsPanelPresenter._
import io.udash.properties.HasModelPropertyCreator
import io.udash.properties.seq.{ReadableSeqProperty, SeqProperty}
import io.udash.properties.single.CastableProperty


/**
  * Why own properties:
  * - Using GenCodes nice for rest RPC
  * - Unable to use sealed hierarchy in Udash Properties
  * - Adding HProperty functionality
  */
object ObjectsPanelPresenter {


  sealed trait ActionStatus

  case object Pending extends ActionStatus

  case object Success extends ActionStatus

  case object Failed extends ActionStatus

  case class Action(status: ActionStatus, description: String)

  sealed trait Resource {
    def name: String
  }

  case class SingleResource(name: String, value: String, lastAction: Opt[Action] = Opt.Empty) extends Resource

  case class MultiResource(name: String, value: Map[Int, String], lastAction: Opt[Action] = Opt.Empty) extends Resource

  case class ObjectInstance(id: Int, resources: Seq[Resource], lastAction: Opt[Action] = Opt.Empty)

  case class SomeObject(name: String, instances: Seq[ObjectInstance], lastAction: Opt[Action] = Opt.Empty)

  case class ObjectsPanelModel(objects: Seq[SomeObject])


  object ObjectInstance extends HasModelPropertyCreator[ObjectInstance]

  object SomeObject extends HasModelPropertyCreator[SomeObject]

  object ObjectsPanelModel extends HasModelPropertyCreator[ObjectsPanelModel]

}

class ObjectsPanelPresenter {
  private val model: SeqProperty[SomeObject, CastableProperty[SomeObject]] = {
    SeqProperty(
      SomeObject("object 1", Vector(
        ObjectInstance(3, Vector(
          SingleResource("resource 1", "value1"),
          MultiResource("multi resource", Map(2 -> "value 2", 3 -> "value 3")))))))
  }

  def getModel: ReadableSeqProperty[SomeObject, CastableProperty[SomeObject]] = model

  def setResourceValue(o: String, instance: Int, resource: String, resourceInstance: Option[Int], value: String): Unit = {
    val resourceModel = model.elemProperties.find(p => p.get.name == o).get.asModel
      .subSeq(_.instances).elemProperties.find(p => p.get.id == instance).get.asModel
      .subSeq(_.resources).elemProperties.find(p => p.get.name == resource).get

    val newVal = resourceModel.get match {
      case s: SingleResource => SingleResource(resource, value, Opt(Action(Success, s"value set: $value")))
      case m: MultiResource =>
        val newMap = m.value + (resourceInstance.get -> value)
        MultiResource(resource, newMap, Opt(Action(Success, s"value set: ${resourceInstance.get} -> $value")))
    }

    resourceModel.set(newVal)
  }

  def addObject(o: String): Unit = {
    model.append(SomeObject(o, Seq.empty))
  }

  def addInstance(o: String, i: Int): Unit = {
    val objectModel = model.elemProperties.find(p => p.get.name == o).get.asModel
    objectModel.subSeq(_.instances).append(ObjectInstance(i, Seq.empty))
    objectModel.subProp(_.lastAction).set(Opt(Action(Success, s"instance added: $i")))
  }

  def addResource(o: String, i: Int, r: String, value: String): Unit = {
    model.elemProperties.find(p => p.get.name == o).get.asModel
      .subSeq(_.instances).elemProperties.find(p => p.get.id == i).get.asModel
      .subSeq(_.resources).append(SingleResource(r, value))
  }

  def removeObject(o: String): Unit = {
    val idx = model.get.indexWhere(_.name == o)
    model.remove(idx, 1)
  }
}
