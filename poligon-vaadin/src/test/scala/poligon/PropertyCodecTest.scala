package poligon

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

  test("ser deser") {
    val prop = FajnyModel.propertyCodec.newProperty(m)
    val deser = FajnyModel.propertyCodec.readProperty(prop)
    println(Property.print(prop))
    assert(m == deser)
  }

  test("update") {
    val prop = FajnyModel.propertyCodec.newProperty(m)
    val newValue = FajnyModel(
      2,
      "string2",
      2.4,
      MalyModel(13),
      Case1(32),
      BardzoProsty("bardzo prosty 2"),
      Case2("aaa2", Vector(Instant.now(), Instant.now().plusSeconds(10))))
    FajnyModel.propertyCodec.updateProperty(newValue, prop)
    val deser = FajnyModel.propertyCodec.readProperty(prop)
    println(Property.print(prop))
    assert(newValue == deser)
  }

  test("get sub property") {
    val prop: Property[FajnyModel] = PropertyCodec.newProperty(m)
    val subProp = SubProperty.getField(prop)(_.ref(_.m))
    val sub = PropertyCodec.readProperty(subProp)
    assert(m.m == sub)
  }
}
