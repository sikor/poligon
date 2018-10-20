import io.udash.properties.HasModelPropertyCreator
import io.udash.properties.seq.SeqProperty
import org.scalatest.FunSuite

class UdashPropertiesTest extends FunSuite {


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


  test("Property binding") {
    val model =
      SeqProperty(
        SomeObject("object 1", Vector(
          ObjectInstance(3, Vector(
            SingleResource("resource 1", "value1"),
            MultiResource("multi resource", Map(2 -> "value 2", 3 -> "value 3")))))))

    val instances = model.elemProperties.find(o => o.get.name == "object 1")
      .get.asModel.subSeq(_.instances)
    val resourceModel = instances.elemProperties.find(p => p.get.id == 3).get.asModel
      .subSeq(_.resources).elemProperties.find(p => p.get.name == "resource 1").get

    resourceModel.set(SingleResource("new", "new value"))
  }
}

