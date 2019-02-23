package poligon.exampleapp.services

import poligon.tran.{TranslationKey, Translator}

import scala.concurrent.Future

class FutureTranslator extends Translator[Future] {
  def translate(key: TranslationKey): Future[String] = Future.successful(key.key)
}
