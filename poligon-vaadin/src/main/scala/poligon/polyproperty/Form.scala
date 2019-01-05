package poligon
package polyproperty

/**
  * Obsługa pola w formularzu: Property zawiera aktualną wartość w bazie danych, Form zawiera wartość użytkownika.
  * Jeżeli użytkownik zatwierdził swoją wartość nie widząc najnowszej wartości z bazy to presenter może zwrócić błąd i
  * gui może zapytać użytkownika czy nadpisać. W przeciwnym razie wartość trafia do bazy i properties'a.
  *
  * Obsługa listy w formularzu: Każdy element listy jest podformualrzem. Jeżeli przyjdzie nowa lista z backendu to
  * aktualizujemy wartość w propertiesie a w formularzu mamy dalej starą wartość w gui mamy wtedy takie opcje:
  * 1. możemy poinformować użytkownika, że ktoś już to zmienił i żeby zachował swoje dane i wyświetlił nową wartość
  * 2. Zakładając, że elementy są unikalnie identyfikowalne (powinny być jeżeli są w bazie),
  *    możemy traktować elementy usunięte w backendzie a dalej istniejące w formularzu jako nowo dodane,
  *    natomiast nowo dodane odpowiednio połączyć z tymi z formularza na podstawie identyfiaktorów. Jeżeli kolejność nie jest
  *    ustalona na podstawie identyfikatorów to pokazać w formularzu, które kolejności się zmieniły.
  *
  * Tip: Wymaganie zapisywania bardziej granularnych zmian pozwala uniknąć konfliktów. np max dodanie jednego elementu na raz,
  * albo zmiana tylko jednego pola na raz.
  *
  * Wymagać aby elementy SeqProperty były unikalne. Zmienić na OrderedSetProperty albo na OrderedMapProperty aby łatwiej szukać po,
  * identyfikatorach. Tak na prawdę jest podobne do RecordProperty tylko record ma stała liczbę pól ale różne typy a tutaj mamy ten
  * sam typ ale zmienną liczbę pól. Union z kolei ma zawsze jedno pole ale ze zmiennym typem.
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
