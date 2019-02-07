package poligon
package polyproperty


import com.avsystem.commons.annotation.positioned
import com.avsystem.commons.meta._
import com.avsystem.commons.misc.{ApplierUnapplier, ValueOf}
import com.avsystem.commons.serialization.GenRef
import poligon.polyproperty.Property.Diff.{NoOp, Val}
import poligon.polyproperty.Property._
import poligon.polyproperty.PropertyCodec.PropertyChange._
import poligon.polyproperty.PropertyCodec.StructuralPropertyCodec.StructuralChange
import poligon.polyproperty.PropertyCodec.{PropertyChange, StructuralPropertyCodec}

import scala.collection.mutable.ArrayBuffer
import scala.collection.{SortedMap, mutable}


sealed trait PropertyCodec[T] {
  type PropertyType <: Property[T]

  def newProperty(value: T): PropertyType

  /**
    * Return all properties that were changed and removed.
    * All ancestors of property has non-structural change that must be handled by caller.
    * Returns information about which properties are changed (including argument property) in the bottom up order.
    */
  def updateProperty(value: T, property: PropertyType): Seq[PropertyChange]

  def readProperty(property: PropertyType): T
}

object PropertyCodec {

  sealed trait PropertyChange {
    def property: Property[_]
  }

  object PropertyChange {

    sealed trait Modification[K, V] {
      def map[V2](f: V => V2): Modification[K, V2]
      def entry: Entry[K, V]
    }

    case class Removed[K, V](entry: Entry[K, V]) extends Modification[K, V] {
      def map[V2](f: V => V2): Modification[K, V2] = Removed(entry.map(f))
    }

    case class Added[K, V](entry: Entry[K, V]) extends Modification[K, V] {
      def map[V2](f: V => V2): Modification[K, V2] = Added(entry.map(f))
    }

    case class Entry[K, V](index: Int, key: K, value: V) {
      def map[V2](f: V => V2): Entry[K, V2] = Entry(index, key, f(value))
    }

    type EntryPatch[K, V] = Seq[Modification[K, V]]

    class ValueChange(val property: Property[_]) extends PropertyChange

  }

  sealed trait StructuralPropertyCodec[K, V, T] extends PropertyCodec[T] {
    type StructuralChangeType = StructuralChange[K, V, T]

    protected def createStructuralChange(property: Property[T], modifications: EntryPatch[K, Property[V]]): StructuralChangeType = {
      new StructuralChange[K, V, T](property, modifications)
    }

    def getEntries(p: PropertyType): Seq[Entry[K, Property[V]]]
  }

  object StructuralPropertyCodec {

    class StructuralChange[K, V, T] private[StructuralPropertyCodec](val property: Property[T],
                                                                     val modifications: EntryPatch[K, Property[V]])
      extends PropertyChange

  }

  implicit val booleanCodec: SimplePropertyCodec[Boolean] = SimplePropertyCodec.materialize[Boolean]
  implicit val stringCodec: SimplePropertyCodec[String] = SimplePropertyCodec.materialize[String]
  implicit val byteCodec: SimplePropertyCodec[Byte] = SimplePropertyCodec.materialize[Byte]
  implicit val charCodec: SimplePropertyCodec[Char] = SimplePropertyCodec.materialize[Char]
  implicit val shortCodec: SimplePropertyCodec[Short] = SimplePropertyCodec.materialize[Short]
  implicit val intCodec: SimplePropertyCodec[Int] = SimplePropertyCodec.materialize[Int]
  implicit val longCodec: SimplePropertyCodec[Long] = SimplePropertyCodec.materialize[Long]
  implicit val floatCodec: SimplePropertyCodec[Float] = SimplePropertyCodec.materialize[Float]
  implicit val doubleCodec: SimplePropertyCodec[Double] = SimplePropertyCodec.materialize[Double]

  private def someCodec[T: PropertyCodec]: RecordPropertyCodec[Some[T]] = RecordPropertyCodec.materialize[Some[T]]

  private val noneCodec: SimplePropertyCodec[None.type] = SimplePropertyCodec.materialize[None.type]

  implicit def optionCodec[T: PropertyCodec]: UnionPropertyCodec[Option[T]] = new UnionPropertyCodec[Option[T]](
    List(new Predefined[Some[T]]("Some", implicitly[ClassTag[Some[T]]], someCodec[T]),
      new Predefined[None.type]("None", implicitly[ClassTag[None.type]], noneCodec))
  )

  private val noOpCodec: SimplePropertyCodec[NoOp.type] = SimplePropertyCodec.materialize[NoOp.type]

  private def valCodec[T: PropertyCodec]: RecordPropertyCodec[Val[T]] = RecordPropertyCodec.materialize[Val[T]]

  implicit def diffCodec[T: PropertyCodec]: UnionPropertyCodec[Diff[T]] = new UnionPropertyCodec[Diff[T]](
    List(new Predefined[Val[T]]("Val", implicitly[ClassTag[Val[T]]], valCodec[T]),
      new Predefined[NoOp.type]("NoOp", implicitly[ClassTag[NoOp.type]], noOpCodec))
  ) {
    override def updateProperty(value: Diff[T], property: UnionProperty[Diff[T]]): Seq[PropertyChange] = {
      value match {
        case NoOp => Seq.empty
        case _: Val[T] => super.updateProperty(value, property)
      }
    }
  }

  implicit def seqCodec[E: PropertyCodec]: SeqPropertyCodec[E] = new SeqPropertyCodec[E]()

  implicit def sortedMapCodec[K: Ordering, V: PropertyCodec]: SortedMapPropertyCodec[K, V] = new SortedMapPropertyCodec[K, V]()

  def apply[T](implicit v: PropertyCodec[T]): PropertyCodec[T] = v

  def newProperty[T: PropertyCodec](value: T): Property[T] = PropertyCodec[T].newProperty(value)

  def updateProperty[T: PropertyCodec](value: T, property: Property[T]): Seq[PropertyChange] = {
    val codec = PropertyCodec[T]
    codec.updateProperty(value, property.asInstanceOf[codec.PropertyType])
  }

  def readProperty[T: PropertyCodec](property: Property[T]): T = {
    val codec = PropertyCodec[T]
    codec.readProperty(property.asInstanceOf[codec.PropertyType])
  }

  def simpleCodec[T]: SimplePropertyCodec[T] = new SimplePropertyCodec[T]
}

class SimplePropertyCodec[T] extends PropertyCodec[T] {
  override type PropertyType = SimpleProperty[T]

  override def newProperty(value: T): SimpleProperty[T] =
    new SimpleProperty[T](value)

  override def updateProperty(value: T, property: SimpleProperty[T]): Seq[PropertyChange] = {
    if (property.value != value) {
      property.value = value
      Seq(new ValueChange(property))
    } else {
      Seq.empty
    }
  }

  override def readProperty(property: SimpleProperty[T]): T =
    property.value
}

object SimplePropertyCodec {
  def materialize[T]: SimplePropertyCodec[T] = new SimplePropertyCodec[T]
}

class SeqPropertyCodec[E](implicit val elementCodec: PropertyCodec[E]) extends StructuralPropertyCodec[Int, E, Seq[E]] {
  override type PropertyType = SeqProperty[E]

  override def newProperty(value: Seq[E]): SeqProperty[E] = {
    val property = new SeqProperty[E](new ArrayBuffer)
    value.foreach { v =>
      val pwc = elementCodec.newProperty(v)
      property.value += pwc
    }
    property
  }

  override def updateProperty(value: Seq[E], property: SeqProperty[E]): Seq[PropertyChange] = {
    val childrenUpdates = new ArrayBuffer[PropertyChange]
    val oldSize = property.value.size
    if (value.size <= oldSize) {
      value.zipWithIndex.foreach { case (v, index) =>
        childrenUpdates ++= elementCodec.updateProperty(v, property.value(index).asInstanceOf[elementCodec.PropertyType])
      }
      val removed = property.value.slice(value.size, property.value.size - value.size)
      property.value.remove(value.size, property.value.size - value.size)
      if (removed.nonEmpty) {
        val changes = removed.reverseIterator.zipWithIndex
          .map { case (r, i) =>
            val idx = oldSize - i - 1
            Removed(Entry(idx, idx, r))
          }.toSeq
        childrenUpdates += createStructuralChange(property, changes)
      } else if (childrenUpdates.nonEmpty) {
        childrenUpdates += new ValueChange(property)
      }
    } else {
      value.zipWithIndex.takeWhile(e => e._2 < oldSize).foreach { case (v, index) =>
        childrenUpdates ++= elementCodec.updateProperty(v, property.value(index).asInstanceOf[elementCodec.PropertyType])
      }
      val toInsert: Seq[Property[E]] = value.iterator.slice(oldSize, value.size)
        .map(e => elementCodec.newProperty(e)).toSeq
      property.value.insertAll(oldSize, toInsert)
      if (toInsert.nonEmpty) {
        val change = toInsert.iterator.zipWithIndex.map { case (p, i) =>
          val idx = oldSize + i
          Added(Entry(idx, idx, p))
        }.toSeq
        childrenUpdates += createStructuralChange(property, change)
      }
      else if (childrenUpdates.nonEmpty) {
        childrenUpdates += new ValueChange(property)
      }
    }
    childrenUpdates
  }

  override def readProperty(property: SeqProperty[E]): Seq[E] = {
    property.value.map(v => elementCodec.readProperty(v.asInstanceOf[elementCodec.PropertyType]))
  }

  def insert(property: SeqProperty[E], idx: Int, value: Seq[E]): Seq[PropertyChange] = {
    val newValues: BSeq[Property[E]] = value.map(e => elementCodec.newProperty(e))
    property.value.insert(idx, newValues: _*)
    if (value.nonEmpty) {
      Seq(createStructuralChange(property, newValues.iterator.zipWithIndex.map { case (r, i) =>
        val id = idx + i
        PropertyChange.Added(Entry(id, id, r))
      }.toSeq))
    } else {
      Seq.empty
    }
  }

  def append(property: SeqProperty[E], value: Seq[E]): Seq[PropertyChange] = {
    insert(property, property.value.size, value)
  }

  def remove(property: SeqProperty[E], idx: Int, count: Int): Seq[PropertyChange] = {
    val oldSize = property.value.size
    require(count >= 0 && idx + count <= oldSize)
    val removed = property.value.slice(idx, idx + count)
    property.value.remove(idx, count)
    if (count > 0) {
      Seq(createStructuralChange(property, removed.reverseIterator.zipWithIndex.map { case (r, i) =>
        val id = oldSize - i - 1
        Removed(Entry(id, id, r))
      }.toSeq))
    } else {
      Seq.empty
    }
  }

  def getEntries(p: SeqProperty[E]): Seq[Entry[Int, Property[E]]] =
    p.value.iterator.zipWithIndex.map { case (e, i) => Entry(i, i, e) }.toSeq
}


object SeqPropertyCodec {
  def apply[E](implicit spc: SeqPropertyCodec[E]): SeqPropertyCodec[E] = spc
}

class MapPropertyCodec[K, V](implicit val elementCodec: PropertyCodec[V]) extends StructuralPropertyCodec[K, V, BMap[K, V]] {
  type PropertyType = MapProperty[K, V]

  def newProperty(value: BMap[K, V]): PropertyType = {
    val property = new MapProperty[K, V](new SeqMap)
    value.zipWithIndex.foreach { case (v, i) =>
      val value: Property[V] = elementCodec.newProperty(v._2)
      val key: K = v._1
      property.value.append(key, value)
    }
    property
  }

  def updateProperty(value: BMap[K, V], property: PropertyType): Seq[PropertyChange] = {
    val childrenUpdates = new ArrayBuffer[PropertyChange]
    val thisUpdates = property.value.update(
      value.keys,
      (k, v) => {
        childrenUpdates ++= elementCodec.updateProperty(value(k), v.asInstanceOf[elementCodec.PropertyType])
      },
      k => elementCodec.newProperty(value(k)))
    if (thisUpdates.nonEmpty) {
      childrenUpdates :+ createStructuralChange(property, thisUpdates)
    } else {
      if (childrenUpdates.nonEmpty) {
        childrenUpdates :+ new ValueChange(property)
      } else {
        Seq.empty
      }
    }
  }

  def readProperty(property: PropertyType): BMap[K, V] = {
    val lhm = new mutable.LinkedHashMap[K, V]
    property.value.foreach { case (k, v) => lhm.put(k, elementCodec.readProperty(v.asInstanceOf[elementCodec.PropertyType])) }
    lhm
  }

  def getEntries(p: MapProperty[K, V]): Seq[Entry[K, Property[V]]] = p.value.entries()
}

class SortedMapPropertyCodec[K, V](implicit val elementCodec: PropertyCodec[V], val ordering: Ordering[K])
  extends StructuralPropertyCodec[K, V, SortedMap[K, V]] {

  type PropertyType = SortedMapProperty[K, V, SortedMap[K, V]]

  def newProperty(value: SortedMap[K, V]): PropertyType = {
    val valueProperties = value.mapValues(v => elementCodec.newProperty(v))
    new SortedMapProperty[K, V, SortedMap[K, V]](new SeqSortedMap[K, Property[V]](valueProperties))
  }

  def updateProperty(value: SortedMap[K, V], property: PropertyType): Seq[PropertyChange] = {
    val childrenUpdates = new ArrayBuffer[PropertyChange]
    val thisUpdates = property.value.update(
      value.keys.toSeq,
      (k, v) => {
        childrenUpdates ++= elementCodec.updateProperty(value(k), v.asInstanceOf[elementCodec.PropertyType])
      },
      k => elementCodec.newProperty(value(k)))
    addThisUpdates(property, childrenUpdates, thisUpdates)
  }

  def readProperty(property: PropertyType): SortedMap[K, V] = {
    val tree = new mutable.TreeMap[K, V]
    property.value.foreach { case (k, v) => tree.put(k, elementCodec.readProperty(v.asInstanceOf[elementCodec.PropertyType])) }
    tree
  }

  def put(key: K, value: V, property: PropertyType): Seq[PropertyChange] = {
    val childrenUpdates = new ArrayBuffer[PropertyChange]
    val thisUpdates = property.value.put(
      key,
      (_, v) => {
        childrenUpdates ++= elementCodec.updateProperty(value, v.asInstanceOf[elementCodec.PropertyType])
      },
      _ => elementCodec.newProperty(value))
    addThisUpdates(property, childrenUpdates, thisUpdates)
  }

  def remove(key: K, property: PropertyType): Seq[PropertyChange] = {
    val thisUpdates = property.value.remove(key)
    addThisUpdates(property, Seq.empty, thisUpdates)
  }

  private def addThisUpdates(property: SortedMapProperty[K, V, BSortedMap[K, V]],
                             childrenUpdates: Seq[PropertyChange],
                             thisUpdates: EntryPatch[K, Property[V]]): BSeq[PropertyChange] = {
    if (thisUpdates.nonEmpty) {
      childrenUpdates :+ createStructuralChange(property, thisUpdates)
    } else {
      if (childrenUpdates.nonEmpty) {
        childrenUpdates :+ new ValueChange(property)
      } else {
        Seq.empty
      }
    }
  }

  def getEntries(p: SortedMapProperty[K, V, BSortedMap[K, V]]): Seq[Entry[K, Property[V]]] = p.value.entries()
}

@positioned(positioned.here) class UnionPropertyCodec[T](
                                                          @multi @adtCaseMetadata val cases: List[UnionPropertyCase[_]]
                                                        ) extends StructuralPropertyCodec[String, AnyRef, T] {
  override type PropertyType = UnionProperty[T]

  override def newProperty(value: T): UnionProperty[T] = {
    val thiCase = caseForValue(value)
    val caseCodec = thiCase.propertyCodec.asInstanceOf[PropertyCodec[T]]
    new UnionProperty[T](thiCase.name, caseCodec.newProperty(value))
  }

  def caseForValue(value: T): UnionPropertyCase[_] = {
    cases.findOpt(_.isInstance(value)).getOrElse(throw new Exception(s"Unknown case: $value"))
  }

  override def updateProperty(value: T, property: UnionProperty[T]): Seq[PropertyChange] = {
    val newCase = caseForValue(value)
    val caseCodec = newCase.propertyCodec.asInstanceOf[PropertyCodec[T]]

    if (property.caseName == newCase.name) {
      val changeBuilder = new ArrayBuffer[PropertyChange]
      changeBuilder ++= caseCodec.updateProperty(value, property.value.asInstanceOf[caseCodec.PropertyType])
      if (changeBuilder.nonEmpty) {
        changeBuilder += new ValueChange(property)
      }
      changeBuilder.result()
    } else {
      val oldCaseName = property.caseName
      val oldValue = property.value
      property.caseName = newCase.name
      property.value = caseCodec.newProperty(value)
      val change =
        Seq(Removed(Entry(0, oldCaseName, oldValue.asInstanceOf[Property[AnyRef]])), Added(Entry(0, property.caseName, property.value.asInstanceOf[Property[AnyRef]])))
      Vector(createStructuralChange(property, change))
    }
  }

  override def readProperty(property: UnionProperty[T]): T = {
    val thiCase = caseForName(property.caseName)
      .propertyCodec.asInstanceOf[PropertyCodec[T]]
    thiCase.readProperty(property.value.asInstanceOf[thiCase.PropertyType])
  }

  def caseForName(name: String): UnionPropertyCase[_] = {
    cases.findOpt(_.name == name)
      .getOrElse(throw new Exception(s"Unknown case: $name"))
  }

  def getEntries(p: UnionProperty[T]): Seq[Entry[String, Property[AnyRef]]] = Seq(Entry(0, p.caseName, p.value.asInstanceOf[Property[AnyRef]]))
}

object UnionPropertyCodec extends AdtMetadataCompanion[UnionPropertyCodec] {


}

sealed trait UnionPropertyCase[T] extends TypedMetadata[T] {
  def name: String

  def classTag: ClassTag[T]

  def propertyCodec: PropertyCodec[T]

  def isInstance(value: Any): Boolean =
    classTag.runtimeClass.isInstance(value.asInstanceOf[AnyRef])
}

@positioned(positioned.here) class Predefined[T](
                                                  @reifyName val name: String,
                                                  @infer val classTag: ClassTag[T],
                                                  @infer @checked val propertyCodec: PropertyCodec[T]
                                                ) extends UnionPropertyCase[T]

@positioned(positioned.here) class CaseClass[T](
                                                 @reifyName val name: String,
                                                 @infer val classTag: ClassTag[T],
                                                 @composite val propertyCodec: RecordPropertyCodec[T]
                                               ) extends UnionPropertyCase[T] {
}

@positioned(positioned.here) class CaseObject[T](
                                                  @reifyName val name: String,
                                                  @infer val classTag: ClassTag[T],
                                                  @infer @checked val singletonValue: ValueOf[T],
                                                  @composite val propertyCodec: SimplePropertyCodec[T]
                                                ) extends UnionPropertyCase[T] {
}


@positioned(positioned.here) class RecordPropertyCodec[T](
                                                           @multi @adtParamMetadata val fields: List[RecordPropertyField[_]],
                                                           @infer @checked val unapplier: ApplierUnapplier[T]
                                                         ) extends PropertyCodec[T] {
  type PropertyType = RecordProperty[T]

  val rawRefCreator: GenRef.Creator[T] = GenRef.create[T]

  override def newProperty(value: T): RecordProperty[T] = {
    val property = new RecordProperty[T](new mutable.LinkedHashMap())
    val map = property.fields
    fields.zip(unapplier.unapply(value)).foreach {
      case (field, fieldValue) =>
        val castedField = field.asInstanceOf[RecordPropertyField[Any]]
        val subProperty = castedField.propertyCodec.newProperty(fieldValue)
        map += (field.name -> subProperty)
    }
    property
  }

  override def updateProperty(value: T, property: RecordProperty[T]): Seq[PropertyChange] = {
    val changeBuilder = new ArrayBuffer[PropertyChange]
    val map = property.fields
    fields.zip(unapplier.unapply(value)).foreach {
      case (field, newFieldValue) =>
        val fieldProperty = map(field.name)
        val codec = field.propertyCodec.asInstanceOf[PropertyCodec[Any]]
        changeBuilder ++= codec.updateProperty(newFieldValue, fieldProperty.asInstanceOf[codec.PropertyType])
    }
    if (changeBuilder.nonEmpty) {
      changeBuilder += new ValueChange(property)
      changeBuilder
    } else {
      Seq.empty
    }
  }

  override def readProperty(property: RecordProperty[T]): T = {
    val map = property.fields
    val fieldValues = fields.zip(map.values).map {
      case (field, fieldProperty) =>
        val codec = field.propertyCodec
        codec.readProperty(fieldProperty.asInstanceOf[codec.PropertyType])
    }
    unapplier.apply(fieldValues)
  }
}

object RecordPropertyCodec extends AdtMetadataCompanion[RecordPropertyCodec]

class RecordPropertyField[T](
                              @reifyName val name: String,
                              @infer val propertyCodec: PropertyCodec[T]
                            ) extends TypedMetadata[T]


trait UnionPropertyCodecImplicits[T] {
  def propertyCodec: UnionPropertyCodec[T]
}

abstract class HasUnionPropertyCodec[T](implicit unionCodec: MacroInstances[Unit, UnionPropertyCodecImplicits[T]]) {
  implicit lazy val propertyCodec: UnionPropertyCodec[T] = unionCodec((), this).propertyCodec
}

trait RecordPropertyCodecImplicits[T] {
  def propertyCodec: RecordPropertyCodec[T]
}

abstract class HasRecordPropertyCodec[T](implicit unionCodec: MacroInstances[Unit, RecordPropertyCodecImplicits[T]]) {
  implicit lazy val propertyCodec: RecordPropertyCodec[T] = unionCodec((), this).propertyCodec
}


trait SimplePropertyCodecImplicits[T] {
  def propertyCodec: SimplePropertyCodec[T]
}

abstract class HasSimplePropertyCodec[T](implicit unionCodec: MacroInstances[Unit, SimplePropertyCodecImplicits[T]]) {
  implicit lazy val propertyCodec: SimplePropertyCodec[T] = unionCodec((), this).propertyCodec
}
