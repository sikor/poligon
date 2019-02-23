package poligon.tran

case class TranslationKey(key: String, args: Any*)

object TranslationKey {

  class TranslationKeyOps(val key: String) extends AnyVal {
    def tr: TranslationKey = TranslationKey(key)
  }

}