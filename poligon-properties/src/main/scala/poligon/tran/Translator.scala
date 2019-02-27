package poligon.tran

import monix.eval.Task

trait Translator {

  def translate(key: TranslationKey, lang: String): Task[String]

}
