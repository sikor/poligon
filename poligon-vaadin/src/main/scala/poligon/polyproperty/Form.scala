package poligon
package polyproperty

import com.avsystem.commons.serialization.GenRef

/**
  * Form:
  * - ustawianie wartości
  * - pobieranie aktualnej wartości
  * - walidacja wartości
  * - transformacja wartości (musi być dwukierunkowa)
  * - struktura podobna do Property
  *
  * Use case'y:
  * 1. Komponent może odpowiadać za część formularza nie znając jego reszty
  * 2. Komponent Rodzic jest w stanie pobrać wartości z formularza swoich dzieci
  * 2.1 Rodzic komunikuje się z dziećmi tylko poprzez Form i Property.
  * 3. Przycisk wysyłania może być jednym z dzieci
  * 4. Wartość może mieć walidator
  * 5. Komponent może ustawić wartość w swojej części formularza
  * 6. Obsługa wartości listowych
  * 7. Czy potrzebne jest nasłuchiwanie na wartości - np wartości inicjalne, albo wartości w innej części formularza.
  * (Np rodzic chce ustawić jakąś wartość w formularzu dla swoich dzieci i je odświerzyć)
  * (jeżeli jakiś komponent chce nasłuchiwać na jakieś wartości to można dodać propertiesa do presentera?).
  */
object Form {
  def apply[T: PropertyCodec](value: T): Form[T] =
    new Form[T](PropertyCodec.newProperty[T](value))
}

class Form[S](private val property: Property[S]) {

  def getSubProperty[T: PropertyCodec](ref: GenRef.Creator[S] => GenRef[S, T])
                                      (implicit rpc: RecordPropertyCodec[S]): Form[T] =
    new Form(SubProperty.getField(property)(ref))

  def getCase[T <: S : ClassTag : PropertyCodec](implicit upc: UnionPropertyCodec[S]): Opt[Form[T]] =
    SubProperty.getCase[S, T](property).map(p => new Form[T](p))

  def getSeq[E: PropertyCodec](implicit ev: Property[S] =:= Property[Seq[E]]): Seq[Form[E]] =
    SubProperty.getSeq(ev.apply(property)).map(p => new Form[E](p))

  def get(implicit codec: PropertyCodec[S]): S = property.get

  def set(value: S)(implicit codec: PropertyCodec[S]): Unit = {
    PropertyCodec.updateProperty(value, property)
  }

  def insert[E: SeqPropertyCodec](
                                   index: Int,
                                   value: E*)(
                                   implicit
                                   ev: Form[S] =:= Form[Seq[E]]): Unit = {
    val seqProp = SubProperty.asSeqProperty(ev(this).property)
    SeqPropertyCodec[E].insert(seqProp, index, value)
  }

  def append[E: SeqPropertyCodec](
                                   value: E*)(
                                   implicit
                                   ev: Form[S] =:= Form[Seq[E]]): Unit = {
    val seqProp = SubProperty.asSeqProperty(ev(this).property)
    SeqPropertyCodec[E].append(seqProp, value)
  }

  def remove[E: SeqPropertyCodec](
                                   index: Int,
                                   count: Int
                                 )(
                                   implicit
                                   ev: Form[S] =:= Form[Seq[E]]): Unit = {
    val seqProp = SubProperty.asSeqProperty(ev(this).property)
    SeqPropertyCodec[E].remove(seqProp, index, count)
  }


}
