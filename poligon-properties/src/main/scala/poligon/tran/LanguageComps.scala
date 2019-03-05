package poligon.tran

import poligon.comp.{Comps, GComp}
import poligon.polyproperty.Act.Act
import poligon.polyproperty.Obs.Obs
import poligon.polyproperty.{GAct, GObs}

trait LanguageComps[D <: HasLanguageSupport] {
  comps: Comps[D] =>
  def tranLabel(property: Obs[TranslationKey, D], styleName: String = ""): GComp[D] =
    comps.dynLabel(property.mapAsync(translate))

  def tLabel(property: TranslationKey, styleName: String = ""): GComp[D] =
    tranLabel(GObs.constant(property))

  def translate(key: TranslationKey): Act[String, D] =
    GAct.create(d => d.deps.translator.translate(key, d.deps.language.read))
}
