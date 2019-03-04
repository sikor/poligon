package poligon.tran

import poligon.polyproperty.PropertyWithParent

trait HasLanguageSupport {
  def translator: Translator

  def language: PropertyWithParent[String]
}
