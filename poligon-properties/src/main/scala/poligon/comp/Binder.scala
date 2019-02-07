package poligon.comp

import com.avsystem.commons.misc.OptArg
import poligon.polyproperty.PropertyWithParent.Struct
import poligon.polyproperty._

trait Binder {

  type Comp

  case class BaseSettings(caption: OptArg[String] = OptArg.Empty)

  case class LayoutSettings(layoutType: LayoutType = Vertical, spacing: Boolean = true)

  sealed trait LayoutType

  case object Vertical extends LayoutType

  case object Horizontal extends LayoutType

  case object Form extends LayoutType

  def layout(comps: Comp*)(settings: LayoutSettings = LayoutSettings()): Comp

  def dynLayout[V](
                    property: Obs[Struct[V]],
                    layoutDescription: LayoutSettings = LayoutSettings())(
                    childFactory: PropertyWithParent[V] => Comp): Comp

  def label(value: String, styleName: String = ""): Comp

  def dynLabel(property: Obs[String], styleName: String = ""): Comp

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): Comp

  def textField(caption: String, property: PropertyWithParent[String]): Comp =
    textField(caption, property.read, property.set)


  def button(onClick: Sin[Unit], caption: Obs[String], enabled: Obs[Boolean]): Comp

  def button(onClick: Sin[Unit], caption: String): Comp =
    button(onClick, Obs.constant(caption), Obs.constant(true))

  def checkBox(caption: String, initValue: Boolean, value: Sin[Boolean]): Comp

  def menuBar[T](menuItems: Seq[(List[String], T)], itemSelected: Sin[T]): Comp

  def replaceable(property: Obs[Comp]): Comp
}
