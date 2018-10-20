package databinding

import databinding.ObjectsPanelPresenter.{MultiResource, ObjectInstance, SingleResource, SomeObject}
import io.udash.properties.HasModelPropertyCreator
import io.udash.properties.seq.{ReadableSeqProperty, SeqProperty}
import io.udash.properties.single.CastableProperty

object ObjectsPanelPresenter {

  sealed trait Resource {
    def name: String
  }

  case class SingleResource(name: String, value: String) extends Resource

  //  object SingleResource extends HasModelPropertyCreator[SingleResource]

  case class MultiResource(name: String, value: Map[Int, String]) extends Resource

  //  object MultiResource extends HasModelPropertyCreator[MultiResource]

  case class ObjectInstance(id: Int, resources: Seq[Resource])

  object ObjectInstance extends HasModelPropertyCreator[ObjectInstance]

  case class SomeObject(name: String, instances: Seq[ObjectInstance])

  object SomeObject extends HasModelPropertyCreator[SomeObject]

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
      case s: SingleResource => SingleResource(resource, value)
      case m: MultiResource =>
        val newMap = m.value + (resourceInstance.get -> value)
        MultiResource(resource, newMap)
    }

    resourceModel.set(newVal)
  }

  def addObject(o: String): Unit = {
    model.append(SomeObject(o, Seq.empty))
  }

  def addInstance(o: String, i: Int): Unit = {
    model.elemProperties.find(p => p.get.name == o).get.asModel
      .subSeq(_.instances).append(ObjectInstance(i, Seq.empty))
  }

  def addResource(o: String, i: Int, r: String, value: String): Unit = {
    model.elemProperties.find(p => p.get.name == o).get.asModel
      .subSeq(_.instances).elemProperties.find(p => p.get.id == i).get.asModel
      .subSeq(_.resources).append(SingleResource(r, value))
  }
}
