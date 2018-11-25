import java.time.Instant

import org.scalatest.FunSuite
import poligon.polyproperty._

case class MalyModel(s: Short)

object MalyModel extends HasRecordPropertyCodec[MalyModel]

case class FajnyModel(i: Int, s: String, d: Double, m: MalyModel, ios: IntOrString, prosty: BardzoProsty, ios2: IntOrString)

object FajnyModel extends HasRecordPropertyCodec[FajnyModel]

case class BardzoProsty(s: String)

object BardzoProsty extends HasSimplePropertyCodec[BardzoProsty]

sealed trait IntOrString

case class Case1(i: Int) extends IntOrString

case class Case2(s: String, instants: Seq[Instant]) extends IntOrString

case object NoneOfThis extends IntOrString

object IntOrString extends HasUnionPropertyCodec[IntOrString]

class PropertyCodecTest extends FunSuite {

  val m = FajnyModel(
    1,
    "string",
    2.3,
    MalyModel(12),
    Case2("aaa", Vector(Instant.now(), Instant.now().plusSeconds(10))),
    BardzoProsty("bardzo prosty"),
    NoneOfThis)

  test("new property") {
    val prop = FajnyModel.propertyCodec.newProperty(m)
    println(PropertyWithCodec.print(prop))
  }
}
