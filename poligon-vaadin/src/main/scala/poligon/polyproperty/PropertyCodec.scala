package poligon
package polyproperty

import java.time.Instant

import com.avsystem.commons.annotation.positioned
import com.avsystem.commons.meta._
import com.avsystem.commons.misc.{ApplierUnapplier, ValueOf}
import com.avsystem.commons.serialization.GenRef
import poligon.polyproperty.Property._
import poligon.polyproperty.PropertyCodec.PropertyLifetimeListener
import poligon.polyproperty.PropertyObserver.SeqPatch

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


sealed trait PropertyCodec[T] {
  type PropertyType <: Property[T]

  def newProperty(value: T): PropertyType

  def updateProperty(value: T, property: PropertyType, onChildPropertyChanged: PropertyLifetimeListener): Unit

  def readProperty(property: PropertyType): T
}

object PropertyCodec {

  trait PropertyLifetimeListener {
    def onPropertyChanged(property: Property[_]): Unit

    def onPropertyRemoved(property: Property[_]): Unit
  }

  object NoOpPropertyLifetimeListener extends PropertyLifetimeListener {
    override def onPropertyChanged(property: Property[_]): Unit = {}

    override def onPropertyRemoved(property: Property[_]): Unit = {}
  }

  implicit val stringCodec: SimplePropertyCodec[String] = SimplePropertyCodec.materialize[String]
  implicit val byteCodec: SimplePropertyCodec[Byte] = SimplePropertyCodec.materialize[Byte]
  implicit val charCodec: SimplePropertyCodec[Char] = SimplePropertyCodec.materialize[Char]
  implicit val shortCodec: SimplePropertyCodec[Short] = SimplePropertyCodec.materialize[Short]
  implicit val intCodec: SimplePropertyCodec[Int] = SimplePropertyCodec.materialize[Int]
  implicit val longCodec: SimplePropertyCodec[Long] = SimplePropertyCodec.materialize[Long]
  implicit val floatCodec: SimplePropertyCodec[Float] = SimplePropertyCodec.materialize[Float]
  implicit val doubleCodec: SimplePropertyCodec[Double] = SimplePropertyCodec.materialize[Double]
  implicit val dateCodec: SimplePropertyCodec[JDate] = SimplePropertyCodec.materialize[JDate]
  implicit val instanceCodec: SimplePropertyCodec[Instant] = SimplePropertyCodec.materialize[Instant]

  implicit def seqCodec[E: PropertyCodec]: SeqPropertyCodec[E] = new SeqPropertyCodec[E]()

  def apply[T](implicit v: PropertyCodec[T]): PropertyCodec[T] = v

  def newProperty[T: PropertyCodec](value: T): Property[T] = PropertyCodec[T].newProperty(value)

  def updateProperty[T: PropertyCodec](value: T, property: Property[T], onChildPropertyChanged: PropertyLifetimeListener = NoOpPropertyLifetimeListener): Unit = {
    val codec = PropertyCodec[T]
    codec.updateProperty(value, property.asInstanceOf[codec.PropertyType], onChildPropertyChanged)
  }

  def readProperty[T: PropertyCodec](property: Property[T]): T = {
    val codec = PropertyCodec[T]
    codec.readProperty(property.asInstanceOf[codec.PropertyType])
  }
}

class SimplePropertyCodec[T] extends PropertyCodec[T] {
  override type PropertyType = SimpleProperty[T]

  override def newProperty(value: T): SimpleProperty[T] =
    new SimpleProperty[T](value)

  override def updateProperty(value: T, property: SimpleProperty[T], onChildPropertyChanged: PropertyLifetimeListener): Unit =
    property.value = value

  override def readProperty(property: SimpleProperty[T]): T =
    property.value
}

object SimplePropertyCodec {
  def materialize[T]: SimplePropertyCodec[T] = new SimplePropertyCodec[T]
}

class SeqPropertyCodec[E](implicit val elementCodec: PropertyCodec[E]) extends PropertyCodec[Seq[E]] {
  override type PropertyType = SeqProperty[E]

  override def newProperty(value: Seq[E]): SeqProperty[E] = {
    val property = new SeqProperty[E](new ArrayBuffer)
    value.foreach { v =>
      val pwc = elementCodec.newProperty(v)
      property.value += pwc
    }
    property
  }

  override def updateProperty(value: Seq[E], property: SeqProperty[E], onChildPropertyChanged: PropertyLifetimeListener): Unit = {
    property.value.foreach { p =>
      PropertyMarker.traverseWithChildren(p, onChildPropertyChanged.onPropertyRemoved)
    }
    property.value.clear()
    value.foreach { v =>
      val pwc = elementCodec.newProperty(v)
      property.value += pwc
    }
  }

  override def readProperty(property: SeqProperty[E]): Seq[E] = {
    property.value.map(v => elementCodec.readProperty(v.asInstanceOf[elementCodec.PropertyType]))
  }

  def insert(property: SeqProperty[E], idx: Int, value: Seq[E]): SeqPatch[E] = {
    val newValues = value.map(e => elementCodec.newProperty(e))
    property.value.insert(idx, newValues: _*)
    new SeqPatch(property, idx, newValues, Seq.empty)
  }

  def append(property: SeqProperty[E], value: Seq[E]): SeqPatch[E] = {
    insert(property, property.value.size, value)
  }

  def remove(property: SeqProperty[E], idx: Int, count: Int): SeqPatch[E] = {
    val removed = property.value.slice(idx, idx + count)
    property.value.remove(idx, count)
    new SeqPatch(property, idx, Seq.empty, removed)
  }
}

object SeqPropertyCodec {
  def apply[E](implicit spc: SeqPropertyCodec[E]): SeqPropertyCodec[E] = spc
}

@positioned(positioned.here) class UnionPropertyCodec[T](
                                                          @multi @adtCaseMetadata val cases: List[UnionPropertyCase[_]]
                                                        ) extends PropertyCodec[T] {
  override type PropertyType = UnionProperty[T]

  override def newProperty(value: T): UnionProperty[T] = {
    val thiCase = caseForValue(value)
    val caseCodec = thiCase.propertyCodec.asInstanceOf[PropertyCodec[T]]
    new UnionProperty[T](thiCase.name, caseCodec.newProperty(value))
  }

  def caseForValue(value: T): UnionPropertyCase[_] = {
    cases.findOpt(_.isInstance(value)).getOrElse(throw new Exception(s"Unknown case: $value"))
  }

  override def updateProperty(value: T, property: UnionProperty[T], onChildPropertyChanged: PropertyLifetimeListener): Unit = {
    val thiCase = caseForValue(value)
    val caseCodec = thiCase.propertyCodec.asInstanceOf[PropertyCodec[T]]
    if (property.caseName == thiCase.name) {
      onChildPropertyChanged.onPropertyChanged(property.value)
      caseCodec.updateProperty(value, property.value.asInstanceOf[caseCodec.PropertyType], onChildPropertyChanged)
    } else {
      PropertyMarker.traverseWithChildren(property.value, onChildPropertyChanged.onPropertyRemoved)
      property.caseName = thiCase.name
      property.value = caseCodec.newProperty(value)
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

  override def updateProperty(value: T, property: RecordProperty[T], onChildPropertyChanged: PropertyLifetimeListener): Unit = {
    val map = property.fields
    fields.zip(unapplier.unapply(value)).foreach {
      case (field, newFieldValue) =>
        val fieldProperty = map(field.name)
        onChildPropertyChanged.onPropertyChanged(fieldProperty)
        val codec = field.propertyCodec.asInstanceOf[PropertyCodec[Any]]
        codec.updateProperty(newFieldValue, fieldProperty.asInstanceOf[codec.PropertyType], onChildPropertyChanged)
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
