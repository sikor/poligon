package poligon
package polyproperty

import com.avsystem.commons.serialization.GenRef

class PropertyWithParent[S](property: Property[S], parent: Opt[PropertyWithParent[_]]) {
  def get[T](ref: GenRef.Creator[S] => GenRef[S, T])
            (implicit rpc: RecordPropertyCodec[S]): PropertyWithParent[T] =
    new PropertyWithParent(SubProperty.getField(property)(ref), this.opt)

  def getCase[T <: S : ClassTag](implicit upc: UnionPropertyCodec[S]): Opt[PropertyWithParent[T]] =
    SubProperty.getCase[S, T](property).map(p => new PropertyWithParent[T](p, this.opt))

  def getSeq[E](implicit ev: Property[S] =:= Property[Seq[E]]): Seq[PropertyWithParent[E]] =
    SubProperty.getSeq(ev.apply(property)).map(p => new PropertyWithParent[E](p, this.opt))
}
