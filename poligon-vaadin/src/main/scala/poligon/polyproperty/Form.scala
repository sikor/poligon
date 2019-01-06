package poligon
package polyproperty

/**
  * Form:
  * - ustawianie wartości
  * - pobieranie aktualnej wartości
  * - walidacja wartości
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
  *    (jeżeli jakiś komponent chce nasłuchiwać na jakieś wartości to można dodać propertiesa do presentera?).
  */
class Form[T: PropertyCodec](val property: Property[T]) {
  private var value: Opt[T] = Opt.Empty

  def setValue(value: T): Unit = this.value = value.opt

  def getValue: Opt[T] = value

  def getInitValue: T = property.get

  def getSeq[E: PropertyCodec](implicit ev: T =:= Seq[E]): Seq[Form[E]] = {
    property.getSeq.map(p => new Form(p))
  }

}
