package poligon

import org.scalatest.FunSuite
import poligon.polyproperty.PropertyCodec.PropertyChange.{Added, Modification, Removed}
import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty._

import scala.collection.SortedMap
import scala.collection.immutable.TreeMap

case class MalyModel(s: Short)

object MalyModel extends HasRecordPropertyCodec[MalyModel]

case class FajnyModel(i: Int, s: String, d: Double, m: MalyModel, ios: IntOrString, prosty: BardzoProsty, ios2: IntOrString)

object FajnyModel extends HasRecordPropertyCodec[FajnyModel]

case class BardzoProsty(s: String)

object BardzoProsty extends HasSimplePropertyCodec[BardzoProsty]

sealed trait IntOrString

case class Case1(i: Int) extends IntOrString

case class Case2(s: String, instants: Seq[JDate]) extends IntOrString

case object NoneOfThis extends IntOrString

object IntOrString extends HasUnionPropertyCodec[IntOrString]

class PropertyCodecTest extends FunSuite {

  val m = FajnyModel(
    1,
    "string",
    2.3,
    MalyModel(12),
    Case2("aaa", Vector(new JDate(), new JDate())),
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
      Case2("aaa2", Vector(new JDate(), new JDate())))
    PropertyCodec.updateProperty(newValue, prop)
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

  test("get case") {
    val prop: Property[FajnyModel] = PropertyCodec.newProperty(m)
    val subProp = SubProperty.getField(prop)(_.ref(_.ios))
    val intCase = SubProperty.getCase[IntOrString, Case1](subProp)
    val strCase = SubProperty.getCase[IntOrString, Case2](subProp)
    val noneCase = SubProperty.getCase[IntOrString, NoneOfThis.type](subProp)
    assert(intCase.isEmpty)
    assert(strCase.isDefined)
    assert(noneCase.isEmpty)
    println(Property.print(strCase.get))

    val subProp2 = SubProperty.getField(prop)(_.ref(_.ios2))
    val noneCase2 = SubProperty.getCase[IntOrString, NoneOfThis.type](subProp2)
    assert(noneCase2.isDefined)
  }

  test("get seq") {
    val prop = PropertyCodec.newProperty(Seq(1, 2, 3))
    val seq = SubProperty.getSeq(prop)
    assert(seq.length == 3)
    assert(PropertyCodec.readProperty(seq.head) == 1)
  }

  test("sorted map insert in the middle") {
    sortedMapInsert("aa", 1)
  }

  test("sorted map insert at the end") {
    sortedMapInsert("cc", 2)
  }

  test("sorted map insert at the beginning") {
    sortedMapInsert("00", 0)
  }

  test("sorted map update") {
    implicit val po: PropertyObservers = PropertyObserver.createRoot
    val p = PropertyWithParent[SortedMap[String, String]](TreeMap("a" -> "a", "b" -> "b", "c" -> "c", "d" -> "d", "y" -> "y"))
    PropertyWithParent.listenStructure[String, String, SortedMap[String, String]](p) { ch =>
      assert(ch.modifications.size == 5)
      assertModification(ch.modifications.head, isAdded = true, 0, "0")
      assertModification(ch.modifications(1), isAdded = true, 2, "aa")
      assertModification(ch.modifications(2), isAdded = false, 4, "c")
      assertModification(ch.modifications(3), isAdded = true, 5, "x")
      assertModification(ch.modifications(4), isAdded = false, 6, "y")
    }
    val newValue = TreeMap("0" -> "0", "a" -> "a", "aa" -> "aa", "b" -> "b", "d" -> "d", "x" -> "x")
    p.set(newValue)
    assert(p.read == newValue)
  }

  private def assertModification(m: Modification[String, PropertyWithParent[String]], isAdded: Boolean, index: Int, value: String): Unit = {
    if (isAdded) {
      assert(m.isInstanceOf[Added[String, PropertyWithParent[String]]])
    } else {
      assert(m.isInstanceOf[Removed[String, PropertyWithParent[String]]])
    }
    assert(m.entry.index == index)
    assert(m.entry.key == value)
    assert(m.entry.value.read == value)
  }

  private def sortedMapInsert(key: String, expIndex: Int): Unit = {
    implicit val po: PropertyObservers = PropertyObserver.createRoot
    val p = PropertyWithParent[SortedMap[String, String]](TreeMap("a" -> "a", "b" -> "b"))
    PropertyWithParent.listenStructure[String, String, SortedMap[String, String]](p) { ch =>
      assert(ch.modifications.size == 1)
      assertModification(ch.modifications.head, isAdded = true, expIndex, key)
    }
    p.put(key, key)
    assert(p.read == TreeMap("a" -> "a", "b" -> "b") + (key -> key))
  }
}
