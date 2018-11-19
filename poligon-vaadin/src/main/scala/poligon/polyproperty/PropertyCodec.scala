package poligon
package polyproperty

import com.avsystem.commons.annotation.positioned
import com.avsystem.commons.meta._
import com.avsystem.commons.misc.{Unapplier, ValueOf}
import com.avsystem.commons.serialization._
import com.avsystem.commons.serialization.json.{JsonOptions, JsonStringOutput}
import poligon.polyproperty.Property.{PropertyWithCodec, UnionProperty}


sealed trait PropertyCodec[T] {
  type PropertyType <: Property[T]

  def newProperty(value: T): PropertyType

  def updateProperty(value: T, property: PropertyType): Unit

  def readProperty(property: PropertyType): T
}

object PropertyCodec extends AdtMetadataCompanion[PropertyCodec] {

  @positioned(positioned.here) class UnionPropertyCodec[T](
                                                            @multi @adtCaseMetadata val cases: List[UnionCase2[_]]
                                                          ) extends PropertyCodec[T] {
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

  @positioned(positioned.here) class UnionCase2[T](
                                                    @reifyName val name: String,
                                                    @infer val classTag: ClassTag[T],
                                                    @infer @checked val propertyCodec: PropertyCodec[T]
                                                  ) extends TypedMetadata[T] {
    def isInstance(value: Any): Boolean =
      classTag.runtimeClass.isInstance(value.asInstanceOf[AnyRef])
  }
}

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