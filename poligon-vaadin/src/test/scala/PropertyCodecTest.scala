import poligon.polyproperty.{HasRecordPropertyCodec, HasSimplePropertyCodec, HasUnionPropertyCodec}

case class MalyModel(s: Short)

object MalyModel extends HasRecordPropertyCodec[MalyModel]

case class FajnyModel(i: Int, s: String, d: Double, m: MalyModel, ios: IntOrString)

object FajnyModel extends HasRecordPropertyCodec[FajnyModel]

case class BardzoProsty(s: String)

object BardzoProsty extends HasSimplePropertyCodec[BardzoProsty]

sealed trait IntOrString

case class Case1(i: Int) extends IntOrString

case class Case2(s: String) extends IntOrString

object IntOrString extends HasUnionPropertyCodec[IntOrString]

object PropertyCodecTest {

  val m = FajnyModel(1, "string", 2.3, MalyModel(12), Case2("aaa"))

  val prop = FajnyModel.propertyCodec.newProperty(m)

  println(prop)
}
