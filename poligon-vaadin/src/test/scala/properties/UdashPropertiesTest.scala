package properties

import io.udash.properties.seq.SeqProperty
import org.scalatest.FunSuite

class UdashPropertiesTest extends FunSuite {

  sealed trait Resource

  case class SingleResource(name: String, value: String) extends Resource

  case class MultiResource(name: String, value: Map[Int, String]) extends Resource

  case class ObjectInstance(id: Int, resources: Vector[Resource])

  case class SomeObject(name: String, instances: Vector[ObjectInstance])

  test("Property binding") {
    val prop = SeqProperty(Vector(SomeObject("obj1", Vector.empty)))
    prop.listen(i => println(i), initUpdate = true)
    prop.elemProperties
  }
}

