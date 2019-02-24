package poligon.comp

import com.avsystem.commons.misc.OptArg
import poligon.comp.CompFamily.LayoutModification.{Added, Removed}
import poligon.comp.CompFamily.MenuTree.MenuItem
import poligon.comp.CompFamily.{LayoutModification, LayoutSettings}
import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty._

trait CompFamily[C] {

  type BComp = BindableComp[C]

  def dynamic(factory: PropertyObservers => C): BComp =
    new BindableComp((po: PropertyObservers) => factory(po))

  def layout(property: Obs[Seq[LayoutModification[BindableComp[C]]]],
             layoutDescription: LayoutSettings = LayoutSettings()): BindableComp[C]

  def label(property: Obs[String], styleName: String = ""): BindableComp[C]

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): BindableComp[C]

  def button(onClick: Sin[Unit], caption: Obs[String], enabled: Obs[Boolean]): BindableComp[C]

  def checkBox(caption: String, initValue: Boolean, value: Sin[Boolean]): BindableComp[C]

  def menuBar[T](menuItems: Seq[(List[String], MenuItem[T])], itemSelected: Sin[T]): BindableComp[C]

  def replaceable(property: Obs[BindableComp[C]]): BindableComp[C]
}

object CompFamily {

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

    def toTree[T](list: Seq[(List[String], MenuItem[T])]): MenuNode[T] = {
      list.foldLeft(MenuNode[T](Map.empty)) { (node, item) =>
        addToTree(node, item._1, item._2)
      }
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
}