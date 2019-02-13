package poligon.comp

import com.avsystem.commons.misc.OptArg
import poligon.comp.Comp.LayoutModification.{Added, Removed}
import poligon.polyproperty.{HasSimplePropertyCodec, Obs, PropertyWithParent, Sin}

trait Comp {
  def createComponent(factory: CompFactory): factory.BindableComp
}

object Comp extends HasSimplePropertyCodec[Comp] {

  sealed trait MenuTree[T]

  object MenuTree {

    case class MenuNode[T](children: Map[String, MenuTree[T]]) extends MenuTree[T]

    sealed trait MenuItem[T] extends MenuTree[T]

    case class MenuValue[T](value: T) extends MenuItem[T]

    case class MenuLink[T](href: String) extends MenuItem[T]

    def toList[T](prefix: List[String], tree: MenuTree[T]): Seq[(List[String], MenuItem[T])] = {
      val builder = Seq.newBuilder[(List[String], MenuItem[T])]
      tree match {
        case MenuNode(children) =>
          val tuples = children.flatMap(c => toList(prefix :+ c._1, c._2))
          builder ++= tuples
        case m: MenuItem[T] =>
          Seq((prefix, m))
      }
      builder.result()
    }

    def addToTree[T](tree: MenuNode[T], path: List[String], value: MenuItem[T]): MenuNode[T] = {
      path match {
        case Nil => throw new IllegalArgumentException("path cannot be empty")
        case head :: Nil => MenuNode[T](tree.children + (head -> value))
        case head :: tail =>
          val nextLevelTree = tree.children.get(head).collectFirst { case m: MenuNode[T] => m }
            .getOrElse(MenuNode[T](Map.empty))
          MenuNode[T](tree.children + (head -> addToTree[T](nextLevelTree, tail, value)))
      }
    }

    def toTree[T](list: Seq[(List[String], MenuItem[T])]): Vector[MenuTree[T]] = {
        ???
    }

  }

  case class BaseSettings(caption: OptArg[String] = OptArg.Empty)

  case class LayoutSettings(layoutType: LayoutType = Vertical, spacing: Boolean = true, caption: String = "")

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

  def factory(highLevelFactory: => Comp): Comp = new Comp {
    def createComponent(factory: CompFactory): factory.BindableComp = highLevelFactory.createComponent(factory)
  }

  def dynLayout[V](
                    property: Obs[Seq[LayoutModification[Comp]]],
                    layoutDescription: LayoutSettings = LayoutSettings()): Comp =
    new Comp {
      def createComponent(factory: CompFactory): factory.BindableComp =
        factory.layout(
          property.map(mods => mods.map(mod => mod.map(desc => desc.createComponent(factory)))),
          layoutDescription)
    }

  def dynLabel(property: Obs[String], styleName: String = ""): Comp =
    new Comp {
      def createComponent(factory: CompFactory): factory.BindableComp = factory.label(property, styleName)
    }

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): Comp =
    new Comp {
      def createComponent(factory: CompFactory): factory.BindableComp = factory.textField(caption, initValue, onValueSet)
    }

  def button(onClick: Sin[Unit], caption: Obs[String], enabled: Obs[Boolean]): Comp =
    new Comp {
      def createComponent(factory: CompFactory): factory.BindableComp = factory.button(onClick, caption, enabled)
    }

  def checkBox(caption: String, initValue: Boolean, value: Sin[Boolean]): Comp =
    new Comp {
      def createComponent(factory: CompFactory): factory.BindableComp = factory.checkBox(caption, initValue, value)
    }

  def menuBar[T](menuItems: Seq[(List[String], T)], itemSelected: Sin[T]): Comp =
    new Comp {
      def createComponent(factory: CompFactory): factory.BindableComp = factory.menuBar(menuItems, itemSelected)
    }

  def replaceable(property: Obs[Comp]): Comp =
    new Comp {
      def createComponent(factory: CompFactory): factory.BindableComp =
        factory.replaceable(property.map(c => c.createComponent(factory)))
    }

  def layout(comps: Comp*)(settings: LayoutSettings = LayoutSettings()): Comp =
    dynLayout(
      Obs.constant(comps.iterator.zipWithIndex.map { case (desc, index) => Added(index, desc) }.toSeq),
      settings)

  def label(value: String, styleName: String = ""): Comp =
    dynLabel(Obs.constant(value), styleName)

  def textField(caption: String, property: PropertyWithParent[String]): Comp =
    textField(caption, property.read, property.set)

  def button(onClick: Sin[Unit], caption: String): Comp =
    button(onClick, Obs.constant(caption), Obs.constant(true))
}
