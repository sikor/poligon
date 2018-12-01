package poligon
package polyproperty

import com.avsystem.commons.serialization.GenRef
import com.avsystem.commons.serialization.RawRef.Field
import poligon.polyproperty.Property.{RecordProperty, SeqProperty, UnionProperty}


object SubProperty {
  def getField[S: RecordPropertyCodec, T](property: Property[S])(ref: GenRef.Creator[S] => GenRef[S, T]): Property[T] = {
    //there is record property codec so this should be RecordProperty
    val recordProperty = property.asInstanceOf[RecordProperty[S]]
    val rawRef = ref(RecordPropertyCodec[S].rawRefCreator).rawRef.normalize
    val fieldName = rawRef.nextOpt
      .getOrElse(throw new IllegalArgumentException("Field name not provided"))
    require(!rawRef.hasNext, "only single reference is allowed")
    recordProperty.fields(fieldName.asInstanceOf[Field].name).asInstanceOf[Property[T]]
  }

  def getCase[S: UnionPropertyCodec, T <: S : ClassTag](property: Property[S]): Opt[Property[T]] = {
    val unionProperty = property.asInstanceOf[UnionProperty[S]]
    val thiCase = UnionPropertyCodec[S].caseForName(unionProperty.caseName)
    if (implicitly[ClassTag[T]].runtimeClass.isAssignableFrom(thiCase.classTag.runtimeClass)) {
      unionProperty.value.asInstanceOf[Property[T]].opt
    } else {
      Opt.Empty
    }
  }

  def getSeq[E: SeqPropertyCodec](property: Property[Seq[E]]): Seq[Property[E]] = {
    property.asInstanceOf[SeqProperty[E]].value
  }

  class SubPropertyExt[S](private val property: Property[S]) extends AnyVal {
    def get[T](ref: GenRef.Creator[S] => GenRef[S, T])(implicit rpc: RecordPropertyCodec[S]): Property[T] =
      getField(property)(ref)

    def getCase[T <: S : ClassTag](implicit upc: UnionPropertyCodec[S]): Opt[Property[T]] =
      SubProperty.getCase[S, T](property)
  }

  trait Implicits {
    implicit def subProp[S](property: Property[S]): SubPropertyExt[S] = new SubPropertyExt[S](property)
  }

  object Implicits extends Implicits

}