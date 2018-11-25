package poligon
package polyproperty

import java.time.Instant

import com.avsystem.commons.annotation.positioned
import com.avsystem.commons.meta._
import com.avsystem.commons.misc.{ApplierUnapplier, ValueOf}
import poligon.polyproperty.Property._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


sealed trait PropertyCodec[T] {
  type PropertyType <: Property[T]

  def newProperty(value: T): PropertyType

  def updateProperty(value: T, property: PropertyType): Unit

  def readProperty(property: PropertyType): T
}

object PropertyCodec {
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
}

class SimplePropertyCodec[T] extends PropertyCodec[T] {
  override type PropertyType = SimpleProperty[T]

  override def newProperty(value: T): SimpleProperty[T] = new SimpleProperty[T](value)

  override def updateProperty(value: T, property: SimpleProperty[T]): Unit = property.value = value

  override def readProperty(property: SimpleProperty[T]): T = property.value
}

object SimplePropertyCodec {
  def materialize[T]: SimplePropertyCodec[T] = new SimplePropertyCodec[T]
}

class SeqPropertyCodec[E](implicit val elementCodec: PropertyCodec[E]) extends PropertyCodec[Seq[E]] {
  override type PropertyType = SeqProperty[E]

  override def newProperty(value: Seq[E]): SeqProperty[E] = {
    val property = new SeqProperty[E](new ArrayBuffer)
    value.foreach { v =>
      val pwc = new PropertyWithCodec[E](elementCodec.newProperty(v), elementCodec)
      property.value += pwc
    }
    property
  }

  override def updateProperty(value: Seq[E], property: SeqProperty[E]): Unit = {
    property.value.clear()
    value.foreach { v =>
      val pwc = new PropertyWithCodec[E](elementCodec.newProperty(v), elementCodec)
      property.value += pwc
    }
  }

  override def readProperty(property: SeqProperty[E]): Seq[E] = {
    property.value.map(_.getValue)
  }
}

@positioned(positioned.here) class UnionPropertyCodec[T](
                                                          @multi @adtCaseMetadata val cases: List[UnionPropertyCase[_]]
                                                        ) extends PropertyCodec[T] {
  override type PropertyType = UnionProperty[T]

  override def newProperty(value: T): UnionProperty[T] = {
    val thiCase = cases.findOpt(_.isInstance(value)).getOrElse(throw new Exception(s"Unknown case: $value"))
    val caseCodec = thiCase.propertyCodec.asInstanceOf[PropertyCodec[T]]
    new UnionProperty[T](thiCase.name, new PropertyWithCodec[T](caseCodec.newProperty(value), caseCodec))
  }

  override def updateProperty(value: T, property: UnionProperty[T]): Unit = {
    val thiCase = cases.findOpt(_.isInstance(value)).getOrElse(throw new Exception(s"Unknown case: $value"))
    val caseCodec = thiCase.propertyCodec.asInstanceOf[PropertyCodec[T]]
    if (property.caseName == thiCase.name) {
      property.value.asInstanceOf[PropertyWithCodec[T]].update(value)
    } else {
      property.caseName = thiCase.name
      property.value = new PropertyWithCodec[T](caseCodec.newProperty(value), caseCodec)
    }
  }

  override def readProperty(property: UnionProperty[T]): T = {
    property.value.getValue
  }
}

object UnionPropertyCodec extends AdtMetadataCompanion[UnionPropertyCodec]

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

  override def newProperty(value: T): RecordProperty[T] = {
    val property = new RecordProperty[T](new mutable.LinkedHashMap())
    val map = property.fields
    fields.zip(unapplier.unapply(value)).foreach {
      case (field, fieldValue) =>
        val castedField = field.asInstanceOf[RecordPropertyField[Any]]
        val subProperty = castedField.propertyCodec.newProperty(fieldValue)
        map += (field.name -> new PropertyWithCodec(subProperty, castedField.propertyCodec))
    }
    property
  }

  override def updateProperty(value: T, property: RecordProperty[T]): Unit = {
    val map = property.fields
    fields.zip(unapplier.unapply(value)).foreach {
      case (field, fieldValue) =>
        map(field.name).asInstanceOf[PropertyWithCodec[Any]].update(fieldValue)
    }
  }

  override def readProperty(property: RecordProperty[T]): T = {
    unapplier.apply(property.fields.values.map(p => p.getValue).toSeq)
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
