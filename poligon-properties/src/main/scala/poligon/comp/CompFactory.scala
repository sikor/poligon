package poligon.comp

import poligon.comp.Comp.{LayoutModification, LayoutSettings}
import poligon.polyproperty._

trait CompFactory extends HasBindableComp {

  def layout(property: Obs[Seq[LayoutModification[BindableComp]]],
             layoutDescription: LayoutSettings = LayoutSettings()): BindableComp

  def label(property: Obs[String], styleName: String = ""): BindableComp

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): BindableComp

  def button(onClick: Sin[Unit], caption: Obs[String], enabled: Obs[Boolean]): BindableComp

  def checkBox(caption: String, initValue: Boolean, value: Sin[Boolean]): BindableComp

  def menuBar[T](menuItems: Seq[(List[String], T)], itemSelected: Sin[T]): BindableComp

  def replaceable(property: Obs[BindableComp]): BindableComp
}
