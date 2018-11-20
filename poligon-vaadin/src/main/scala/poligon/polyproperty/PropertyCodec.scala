package poligon
package polyproperty

import java.time.Instant

import com.avsystem.commons.annotation.positioned
import com.avsystem.commons.meta._
import com.avsystem.commons.misc.{ApplierUnapplier, Unapplier, ValueOf}
import com.avsystem.commons.serialization._
import com.avsystem.commons.serialization.json.{JsonOptions, JsonStringOutput}
import poligon.polyproperty.Property._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


sealed trait PropertyCodec[T] {
  type PropertyType <: Property[T]

  def newProperty(value: T): PropertyType

  def updateProperty(value: T, property: PropertyType): Unit

  def readProperty(property: PropertyType): T
}

sealed trait AdtPropertyCodec[T] extends PropertyCodec[T]

object PropertyCodec extends AdtMetadataCompanion[AdtPropertyCodec]{
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
      val pwc = new PropertyWithCodec[E](elementCodec.newProperty(v), elementCodec, v)
      property.value += pwc
    }
    property
  }

  override def updateProperty(value: Seq[E], property: SeqProperty[E]): Unit = {
    property.value.clear()
    value.foreach { v =>
      val pwc = new PropertyWithCodec[E](elementCodec.newProperty(v), elementCodec, v)
      property.value += pwc
    }
  }

  override def readProperty(property: SeqProperty[E]): Seq[E] = {
    property.value.map(_.lastValue)
  }
}

object SeqPropertyCodec {
  implicit def materialize[E](implicit elementCodec: PropertyCodec[E]): SeqPropertyCodec[E] = new SeqPropertyCodec[E]()
}

@positioned(positioned.here) class UnionPropertyCodec[T](
                                                          @multi @adtCaseMetadata val cases: List[UnionPropertyCase[_]]
                                                        ) extends AdtPropertyCodec[T] {
  override type PropertyType = UnionProperty[T]

  override def newProperty(value: T): UnionProperty[T] = {
    val thiCase = cases.findOpt(_.isInstance(value)).getOrElse(throw new Exception(s"Unknown case: $value"))
    val caseCodec = thiCase.propertyCodec.asInstanceOf[PropertyCodec[T]]
    new UnionProperty[T](thiCase.name, new PropertyWithCodec[T](caseCodec.newProperty(value), caseCodec, value))
  }

  override def updateProperty(value: T, property: UnionProperty[T]): Unit = {
    val thiCase = cases.findOpt(_.isInstance(value)).getOrElse(throw new Exception(s"Unknown case: $value"))
    val caseCodec = thiCase.propertyCodec.asInstanceOf[PropertyCodec[T]]
    if (property.caseName == thiCase.name) {
      property.value.asInstanceOf[PropertyWithCodec[T]].update(value)
    } else {
      property.caseName = thiCase.name
      property.value = new PropertyWithCodec[T](caseCodec.newProperty(value), caseCodec, value)
    }
  }

  override def readProperty(property: UnionProperty[T]): T = {
    property.value.lastValue
  }
}

object UnionPropertyCodec extends AdtMetadataCompanion[UnionPropertyCodec]

@positioned(positioned.here) class UnionPropertyCase[T](
                                                         @reifyName val name: String,
                                                         @infer val classTag: ClassTag[T],
                                                         @composite val propertyCodec: PropertyCodec[T]
                                                       ) extends TypedMetadata[T] {
  def isInstance(value: Any): Boolean =
    classTag.runtimeClass.isInstance(value.asInstanceOf[AnyRef])
}

@positioned(positioned.here) class RecordPropertyCodec[T](
                                                           @multi @adtParamMetadata val fields: List[RecordPropertyField[_]],
                                                           @infer @checked val unapplier: ApplierUnapplier[T]
                                                         ) extends AdtPropertyCodec[T] {
  type PropertyType = CaseProperty[T]

  override def newProperty(value: T): CaseProperty[T] = {
    val property = new CaseProperty[T](new mutable.LinkedHashMap())
    val map = property.fields
    fields.zip(unapplier.unapply(value)).foreach {
      case (field, fieldValue) =>
        val castedField = field.asInstanceOf[RecordPropertyField[Any]]
        val subProperty = castedField.propertyCodec.newProperty(fieldValue)
        map += (field.name -> new PropertyWithCodec(subProperty, castedField.propertyCodec, fieldValue))
    }
    property
  }

  override def updateProperty(value: T, property: CaseProperty[T]): Unit = {
    val map = property.fields
    fields.zip(unapplier.unapply(value)).foreach {
      case (field, fieldValue) =>
        map(field.name).asInstanceOf[PropertyWithCodec[Any]].update(fieldValue)
    }
  }

  override def readProperty(property: CaseProperty[T]): T = {
    unapplier.apply(property.fields.values.map(p => p.lastValue).toSeq)
  }
}

object RecordPropertyCodec extends AdtMetadataCompanion[RecordPropertyCodec]

class RecordPropertyField[T](
                              @reifyName val name: String,
                              @infer val propertyCodec: PropertyCodec[T]
                            ) extends TypedMetadata[T]


trait Kodek[T] {
  def write(output: Output, value: T): Unit
}

object Kodek {
  def apply[T](implicit kodek: Kodek[T]): Kodek[T] = kodek

  def writeJson[T: Kodek](value: T): String = {
    val sb = new JStringBuilder
    val o = new JsonStringOutput(sb, JsonOptions.Pretty)
    Kodek[T].write(o, value)
    sb.toString
  }

  def create[T](fun: (Output, T) => Unit): Kodek[T] = fun.apply

  implicit val IntKodek: Kodek[Int] = create(_.writeSimple().writeInt(_))
  implicit val StringKodek: Kodek[String] = create(_.writeSimple().writeString(_))

  implicit def fromFallback[T](implicit fallbackKodek: Fallback[Kodek[T]]): Kodek[T] =
    fallbackKodek.value
}

sealed trait AdtKodek[T] extends Kodek[T]

object AdtKodek extends AdtMetadataCompanion[AdtKodek]

@positioned(positioned.here) class UnionKodek[T](
                                                  @multi @adtCaseMetadata val cases: List[UnionCase[_]]
                                                ) extends AdtKodek[T] {
  def write(output: Output, value: T): Unit = {
    val caseUsed = cases.findOpt(_.isInstance(value))
      .getOrElse(throw new Exception(s"Unknown case: $value"))
    val oo = output.writeObject()
    val wrappedOutput = oo.writeField(caseUsed.name)
    caseUsed.kodek.asInstanceOf[Kodek[T]].write(wrappedOutput, value)
    oo.finish()
  }
}

object UnionKodek extends AdtMetadataCompanion[UnionKodek]

sealed trait UnionCase[T] extends TypedMetadata[T] {
  def name: String

  def classTag: ClassTag[T]

  def kodek: Kodek[T]

  def isInstance(value: Any): Boolean =
    classTag.runtimeClass.isInstance(value.asInstanceOf[AnyRef])
}

@positioned(positioned.here) class CornerCase[T](
                                                  @reifyName val name: String,
                                                  @infer val classTag: ClassTag[T],
                                                  @infer @checked val kodek: Kodek[T]
                                                ) extends UnionCase[T]

@positioned(positioned.here) class RecordCase[T](
                                                  @reifyName val name: String,
                                                  @infer val classTag: ClassTag[T],
                                                  @composite val kodek: RecordKodek[T]
                                                ) extends UnionCase[T]

@positioned(positioned.here) class ObjectCase[T](
                                                  @reifyName val name: String,
                                                  @infer val classTag: ClassTag[T],
                                                  @composite val kodek: SingletonKodek[T]
                                                ) extends UnionCase[T]

@positioned(positioned.here)
@annotated[transparent] class TransparentKodek[T](
                                                   @adtParamMetadata val field: RecordField[_],
                                                   @infer @checked val unapplier: Unapplier[T]
                                                 ) extends AdtKodek[T] {
  def write(output: Output, value: T): Unit = {
    val fieldValue = unapplier.unapply(value).head
    field.kodek.asInstanceOf[Kodek[Any]].write(output, fieldValue)
  }
}

@positioned(positioned.here) class RecordKodek[T](
                                                   @multi @adtParamMetadata val fields: List[RecordField[_]],
                                                   @infer @checked val unapplier: Unapplier[T]
                                                 ) extends AdtKodek[T] {
  def write(output: Output, value: T): Unit = {
    val oo = output.writeObject()
    (fields zip unapplier.unapply(value)).foreach {
      case (field: RecordField[Any@unchecked], fieldValue) =>
        if (!field.transientDefault || !field.defValue.contains(fieldValue)) {
          field.kodek.write(oo.writeField(field.rawName), fieldValue)
        }
      case _ =>
    }
    oo.finish()
  }
}

object RecordKodek extends AdtMetadataCompanion[RecordKodek]

@positioned(positioned.here) class SingletonKodek[T](
                                                      @infer @checked val value: ValueOf[T]
                                                    ) extends AdtKodek[T] with TypedMetadata[T] {
  def write(output: Output, value: T): Unit =
    output.writeObject().finish()
}

class RecordField[T](
                      @reifyName val name: String,
                      @optional @reifyAnnot val annotName: Opt[name],
                      @isAnnotated[transientDefault] val transientDefault: Boolean,
                      @optional @reifyDefaultValue val defaultValue: Opt[DefaultValue[T]],
                      @infer val kodek: Kodek[T]
                    ) extends TypedMetadata[T] {
  def rawName: String = annotName.fold(name)(_.name)

  val defValue: Opt[T] = defaultValue.flatMap(dv => Try(dv.value).toOpt)

}

trait Kodeki[T] {
  def kodek: AdtKodek[T]

  def codec: GenCodec[T]
}

object CustomCodecs {
  implicit val doubleKodek: Fallback[Kodek[Double]] =
    Fallback(Kodek.create(_.writeSimple().writeDouble(_)))
}

abstract class HasKodek[T](
                            implicit macroKodek: MacroInstances[CustomCodecs.type, Kodeki[T]]
                          ) {
  implicit lazy val kodek: Kodek[T] = macroKodek(CustomCodecs, this).kodek
  implicit lazy val codec: GenCodec[T] = macroKodek(CustomCodecs, this).codec
}

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
