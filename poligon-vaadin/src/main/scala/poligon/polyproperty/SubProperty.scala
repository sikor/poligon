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

  def getField[S: RecordPropertyCodec, T](property: Property[S], name: String): Property[T] = {
    //there is record property codec so this should be RecordProperty
    val recordProperty = property.asInstanceOf[RecordProperty[S]]
    recordProperty.fields(name).asInstanceOf[Property[T]]
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

  def getSeq[E](property: Property[Seq[E]]): Seq[Property[E]] = {
    asSeqProperty(property).value
  }

  def asSeqProperty[E](property: Property[Seq[E]]): SeqProperty[E] = {
    property.asInstanceOf[SeqProperty[E]]
  }
}