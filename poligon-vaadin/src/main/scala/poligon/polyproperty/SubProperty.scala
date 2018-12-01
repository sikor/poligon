package poligon
package polyproperty

import com.avsystem.commons.serialization.GenRef
import com.avsystem.commons.serialization.RawRef.Field
import poligon.polyproperty.Property.RecordProperty


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

  class SubPropertyExt[S](private val property: Property[S]) extends AnyVal {
    def get[T](ref: GenRef.Creator[S] => GenRef[S, T])(implicit rpc: RecordPropertyCodec[S]): Property[T] =
      getField(property)(ref)

    def waat: String = "lol"
  }

  trait Implicits {
    implicit def subProp[S](property: Property[S]): SubPropertyExt[S] = new SubPropertyExt[S](property)
  }

  object Implicits extends Implicits

}