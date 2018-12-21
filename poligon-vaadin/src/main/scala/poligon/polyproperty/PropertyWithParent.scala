package poligon
package polyproperty

import com.avsystem.commons.serialization.GenRef
import poligon.polyproperty.PropertyObserver.PropertyObservers

class PropertyWithParent[S](val property: Property[S], val parent: Opt[PropertyWithParent[_]]) {
  def getSubProperty[T](ref: GenRef.Creator[S] => GenRef[S, T])
                       (implicit rpc: RecordPropertyCodec[S]): PropertyWithParent[T] =
    new PropertyWithParent(SubProperty.getField(property)(ref), this.opt)

  def getCase[T <: S : ClassTag](implicit upc: UnionPropertyCodec[S]): Opt[PropertyWithParent[T]] =
    SubProperty.getCase[S, T](property).map(p => new PropertyWithParent[T](p, this.opt))

  def getSeq[E](implicit ev: Property[S] =:= Property[Seq[E]]): Seq[PropertyWithParent[E]] =
    SubProperty.getSeq(ev.apply(property)).map(p => new PropertyWithParent[E](p, this.opt))

  def get(implicit codec: PropertyCodec[S]): S = property.get

  def set(value: S)(implicit observed: PropertyObservers, codec: PropertyCodec[S]): Unit = {
    PropertyChanger.set(this, value)
  }

  def insert[E: SeqPropertyCodec](
                                   index: Int,
                                   value: E*)(
                                   implicit
                                   observed: PropertyObservers,
                                   ev: PropertyWithParent[S] =:= PropertyWithParent[Seq[E]]): Unit = {
    PropertyChanger.insert[E](ev.apply(this), index, value: _*)
  }

  def append[E: SeqPropertyCodec](
                                   value: E*)(
                                   implicit
                                   observed: PropertyObservers,
                                   ev: PropertyWithParent[S] =:= PropertyWithParent[Seq[E]]): Unit = {
    PropertyChanger.append[E](ev.apply(this), value: _*)
  }

  def remove[E: SeqPropertyCodec](
                                   index: Int,
                                   count: Int
                                 )(
                                   implicit
                                   observed: PropertyObservers,
                                   ev: PropertyWithParent[S] =:= PropertyWithParent[Seq[E]]): Unit = {
    PropertyChanger.remove[E](ev.apply(this), index, count)
  }

}

object PropertyWithParent {
  def apply[T: PropertyCodec](value: T): PropertyWithParent[T] =
    new PropertyWithParent[T](PropertyCodec.newProperty[T](value), Opt.Empty)
}