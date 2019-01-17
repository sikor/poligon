package poligon
package polyproperty

import poligon.polyproperty.PropertyObserver.PropertyObservers

class PropertyWithParent[S](val property: Property[S], val parent: Opt[PropertyWithParent[_]])


object PropertyWithParent {
  def apply[T: PropertyCodec](value: T): PropertyWithParent[T] =
    new PropertyWithParent[T](PropertyCodec.newProperty[T](value), Opt.Empty)

  implicit class GeneralPropertyExt[T](p: PropertyWithParent[T])(implicit c: PropertyCodec[T]) {
    def get: T = p.property.get

    def set(value: T)(implicit observed: PropertyObservers): Unit = {
      PropertyChanger.set(p, value)
    }
  }

  implicit class UnionPropertyExt[S](p: PropertyWithParent[S])(implicit c: UnionPropertyCodec[S]) {
    def getCase[T <: S : ClassTag]: Opt[PropertyWithParent[T]] =
      SubProperty.getCase[S, T](p.property).map(caseProperty => new PropertyWithParent[T](caseProperty, p.opt))
  }

  implicit class RecordPropertyExt[T](p: PropertyWithParent[T])(implicit c: RecordPropertyCodec[T]) {

    def getField[S](f: T => S): PropertyWithParent[S] = macro poligon.PropertyMacros.getField[S]

    def internalGetField[S](name: String): PropertyWithParent[S] = {
      new PropertyWithParent[S](SubProperty.getField(p.property, name), p.opt)
    }
  }

  implicit class SeqPropertyExt[T](p: PropertyWithParent[Seq[T]])(implicit c: SeqPropertyCodec[T]) {
    def getSeq: Seq[PropertyWithParent[T]] =
      SubProperty.getSeq(p.property).map(e => new PropertyWithParent[T](e, p.opt))

    def insert(index: Int, value: T*)(implicit observed: PropertyObservers): Unit = {
      PropertyChanger.insert[T](p, index, value: _*)
    }

    def append(value: T*)(implicit observed: PropertyObservers): Unit = {
      PropertyChanger.append[T](p, value: _*)
    }

    def remove(index: Int, count: Int)(implicit observed: PropertyObservers): Unit = {
      PropertyChanger.remove[T](p, index, count)
    }
  }

}