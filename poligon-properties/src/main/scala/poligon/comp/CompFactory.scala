package poligon.comp

import poligon.comp.CompDescription.{LayoutModification, LayoutSettings}
import poligon.polyproperty._

trait CompFactory {

  type Comp

  def layout(property: Obs[Seq[LayoutModification[Comp]]],
             layoutDescription: LayoutSettings = LayoutSettings()): Comp

  def label(property: Obs[String], styleName: String = ""): Comp

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): Comp

  def button(onClick: Sin[Unit], caption: Obs[String], enabled: Obs[Boolean]): Comp

  def checkBox(caption: String, initValue: Boolean, value: Sin[Boolean]): Comp

  def menuBar[T](menuItems: Seq[(List[String], T)], itemSelected: Sin[T]): Comp

  def replaceable(property: Obs[Comp]): Comp
}
