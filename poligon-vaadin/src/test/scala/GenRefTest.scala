import GenRefTest.{ExampleModel, ExampleSubmodel, SubSubModel}
import com.avsystem.commons.serialization.GenRef
import org.scalatest.FunSuite

object GenRefTest {

  case class ExampleModel(list: Vector[ExampleSubmodel], value: ExampleSubmodel, simpleValue: Int)

  case class ExampleSubmodel(subList: List[SubSubModel])

  case class SubSubModel(s: String)

  object ExampleModel extends GenRef.Creator[ExampleModel]

}

class GenRefTest extends FunSuite {

  test("GenRef") {
    val ref = ExampleModel.ref(_.list)
    val em = ExampleModel(
      Vector(ExampleSubmodel(List.empty), ExampleSubmodel(List(SubSubModel("subsub")))),
      ExampleSubmodel(List.empty),
      11)
    val subValue = ref.fun(em)
    println(subValue)
    println(ref.rawRef)
    println(ref.rawRef.normalize.toVector)
  }

}
