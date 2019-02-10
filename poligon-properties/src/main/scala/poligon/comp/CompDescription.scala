package poligon.comp

import com.avsystem.commons.misc.OptArg
import poligon.comp.CompDescription.LayoutModification.{Added, Removed}
import poligon.polyproperty.{Obs, PropertyWithParent, Sin}

trait CompDescription {
  def createComponent(factory: CompFactory): factory.Comp
}

object CompDescription {

  case class BaseSettings(caption: OptArg[String] = OptArg.Empty)

  case class LayoutSettings(layoutType: LayoutType = Vertical, spacing: Boolean = true)

  sealed trait LayoutType

  case object Vertical extends LayoutType

  case object Horizontal extends LayoutType

  case object Form extends LayoutType

  sealed trait LayoutModification[V] {
    def map[V2](f: V => V2): LayoutModification[V2] = this match {
      case Added(index, v) => Added(index, f(v))
      case Removed(index) => Removed(index)
    }
  }

  object LayoutModification {

    case class Added[V](index: Int, comp: V) extends LayoutModification[V]

    case class Removed[V](index: Int) extends LayoutModification[V]

  }

  def dynLayout[V](
                    property: Obs[Seq[LayoutModification[CompDescription]]],
                    layoutDescription: LayoutSettings = LayoutSettings()): CompDescription =
    new CompDescription {
      def createComponent(factory: CompFactory): factory.Comp =
        factory.layout(
          property.map(mods => mods.map(mod => mod.map(desc => desc.createComponent(factory)))),
          layoutDescription)
    }

  def dynLabel(property: Obs[String], styleName: String = ""): CompDescription =
    new CompDescription {
      def createComponent(factory: CompFactory): factory.Comp = factory.label(property, styleName)
    }

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): CompDescription =
    new CompDescription {
      def createComponent(factory: CompFactory): factory.Comp = factory.textField(caption, initValue, onValueSet)
    }

  def button(onClick: Sin[Unit], caption: Obs[String], enabled: Obs[Boolean]): CompDescription =
    new CompDescription {
      def createComponent(factory: CompFactory): factory.Comp = factory.button(onClick, caption, enabled)
    }

  def checkBox(caption: String, initValue: Boolean, value: Sin[Boolean]): CompDescription =
    new CompDescription {
      def createComponent(factory: CompFactory): factory.Comp = factory.checkBox(caption, initValue, value)
    }

  def menuBar[T](menuItems: Seq[(List[String], T)], itemSelected: Sin[T]): CompDescription =
    new CompDescription {
      def createComponent(factory: CompFactory): factory.Comp = factory.menuBar(menuItems, itemSelected)
    }

  def replaceable(property: Obs[CompDescription]): CompDescription =
    new CompDescription {
      def createComponent(factory: CompFactory): factory.Comp =
        factory.replaceable(property.map(c => c.createComponent(factory)))
    }

  def layout(comps: CompDescription*)(settings: LayoutSettings = LayoutSettings()): CompDescription =
    dynLayout(
      Obs.constant(comps.iterator.zipWithIndex.map { case (desc, index) => Added(index, desc) }.toSeq),
      settings)

  def label(value: String, styleName: String = ""): CompDescription =
    dynLabel(Obs.constant(value), styleName)

  def textField(caption: String, property: PropertyWithParent[String]): CompDescription =
    textField(caption, property.read, property.set)

  def button(onClick: Sin[Unit], caption: String): CompDescription =
    button(onClick, Obs.constant(caption), Obs.constant(true))
}
