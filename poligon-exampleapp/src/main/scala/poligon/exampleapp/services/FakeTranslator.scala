package poligon.exampleapp.services

import monix.eval.Task
import poligon.tran.{TranslationKey, Translator}

class FakeTranslator extends Translator {
  def translate(key: TranslationKey, lang: String): Task[String] = Task.now(key.key + s"_$lang")
}
