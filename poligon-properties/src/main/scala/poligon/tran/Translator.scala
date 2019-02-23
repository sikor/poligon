package poligon.tran

trait Translator[A[_]] {

  def translate(key: TranslationKey): A[String]

}
