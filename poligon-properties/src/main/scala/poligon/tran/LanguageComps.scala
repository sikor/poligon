package poligon.tran

import poligon.comp.{Comps, GComp}
import poligon.polyproperty.PropertyObserver.GPropertyObservers
import poligon.polyproperty.{GAct, GObs}

trait LanguageComps[D <: HasLanguageSupport] {
  comps: Comps[D] =>
  def tranLabel(property: GObs[TranslationKey, GPropertyObservers[D]], styleName: String = ""): GComp[D] =
    comps.dynLabel(property.mapAsync(translate))

  def translate(key: TranslationKey): GAct[String, GPropertyObservers[D]] =
    GAct.create(d => d.deps.translator.translate(key, d.deps.language.read))
}
