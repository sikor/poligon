import GenRefTest._
import com.avsystem.commons.serialization.{GenRef, generated}
import org.scalatest.FunSuite

object GenRefTest {

  case class ExampleModel(list: Vector[ExampleSubmodel], value: ExampleSubmodel, simpleValue: Int)

  case class ExampleSubmodel(subList: List[SubSubModel], hierarchy: Hierarchy)

  case class SubSubModel(s: String)

  sealed trait Hierarchy {
    @generated
    def some: Double = 10
  }

  case class Option1(value: Int) extends Hierarchy

  case class Option2(value: String) extends Hierarchy

  object ExampleModel extends GenRef.Creator[ExampleModel]

}

class GenRefTest extends FunSuite {

  test("GenRef") {
    val ref = ExampleModel.ref(_.list)
    val em = ExampleModel(
      Vector(ExampleSubmodel(List.empty, Option1(23)), ExampleSubmodel(List(SubSubModel("subsub")), Option1(10))),
      ExampleSubmodel(List.empty, Option2("dwa")),
      11)
    val subValue = ref.fun(em)
    println(subValue)
    println(ref.rawRef)
    println(ref.rawRef.normalize.toVector)

    val ref2 = ExampleModel.ref(_.value.hierarchy.some)
    println(ref2)
  }

}
