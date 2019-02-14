package poligon.scalajscomp

import com.avsystem.commons.SharedExtensions.MapOps.Entry
import org.scalajs.dom
import org.scalajs.dom.Element
import org.scalajs.dom.raw.Node
import poligon.comp.Comp.LayoutModification.{Added, Removed}
import poligon.comp.Comp.MenuTree.{MenuItem, MenuLink, MenuNode, MenuValue}
import poligon.comp.Comp.{Form, Horizontal, LayoutModification, LayoutSettings, MenuTree, Vertical}
import poligon.comp.CompFactory
import poligon.polyproperty.{Obs, Sin}
import scalatags.JsDom.{all => st}
import scalatags.JsDom.all._

object ScalaJsCompFactory extends CompFactory {


  type ComponentT = Element

  trait LayoutBuilder {
    def container: Element

    def addElement(index: Int, element: Element): Unit

    def removeElement(index: Int): Node
  }

  class VerticalLayoutBuilder extends LayoutBuilder {
    val container: Element = dom.document.createElement("div")
    container.setAttribute("class", "container")

    def addElement(index: Int, element: Element): Unit = {
      val row = dom.document.createElement("div")
      row.setAttribute("class", "row")
      val col = dom.document.createElement("div")
      col.setAttribute("class", "col")
      row.appendChild(col)
      if (container.childNodes.length == index) {
        container.appendChild(row)
      } else {
        container.insertBefore(row, container.childNodes(index))
      }
    }

    def removeElement(index: Int): Node = {
      container.removeChild(container.childNodes(index))
    }
  }

  class HorizontalLayoutBuilder extends LayoutBuilder {
    val container: Element = dom.document.createElement("div")
    container.setAttribute("class", "container")
    private val row = dom.document.createElement("div")
    row.setAttribute("class", "row")

    def addElement(index: Int, element: Element): Unit = {
      val col = dom.document.createElement("div")
      col.setAttribute("class", "col")
      if (row.childNodes.length == index) {
        row.appendChild(col)
      } else {
        row.insertBefore(col, row.childNodes(index))
      }
    }

    def removeElement(index: Int): Node = {
      row.removeChild(row.childNodes(index))
    }
  }

  def layout(
              property: Obs[Seq[LayoutModification[BindableComp]]],
              layoutDescription: LayoutSettings): BindableComp =
    dynamic { implicit po =>
      val builder = layoutDescription.layoutType match {
        case Horizontal =>
          new HorizontalLayoutBuilder
        case Vertical | Form =>
          new VerticalLayoutBuilder
      }
      property.listen { modifications =>
        modifications.foreach {
          case Removed(index) =>
            val removedComponent: Node = builder.removeElement(index)
            po.deregisterSubObservers(removedComponent)
          case Added(index, added) =>
            val c = added.looseBind(po)
            builder.addElement(index, c)
        }
      }
      builder.container
    }

  def label(property: Obs[String], styleName: String): BindableComp = dynamic { implicit po =>
    val l = dom.document.createElement("span")
    l.setAttribute("class", styleName)
    property.listen(s => l.innerHTML = s)
    l
  }

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): BindableComp =
    dynamic { implicit po =>
      val input = st.input(st.`type` := "text", st.cls := "form-control", st.id := caption).render
      input.onchange = { _ => onValueSet.push(input.value) }
      st.div(st.cls := "form-group")(
        st.label(st.`for` := caption)("Name:"),
        input
      ).render
    }

  def button(onClick: Sin[Unit], caption: Obs[String], enabled: Obs[Boolean]): BindableComp =
    dynamic { implicit po =>
      val b = st.button(st.cls := "btn").render
      b.onclick = { _ => onClick.push(()) }
      caption.listen(v => b.innerHTML = v)
      enabled.listen(enabled => b.disabled = !enabled)
      b
    }

  def checkBox(caption: String, initValue: Boolean, value: Sin[Boolean]): BindableComp =
    dynamic { implicit po =>
      val in = st.input(st.`type` := "checkbox", st.value := initValue).render
      in.onchange = { _ => value.push(in.value.toBoolean) }
      st.div(cls := "checkbox")(
        st.label(in, "caption")
      ).render
    }

  private def dropDownMenu[T](menuNode: MenuNode[T], onSelect: T => Unit): Element = {
    val menu = st.ul(cls := "dropdown-menu")(
      menuNode.children.entries.map[Modifier] { case Entry(menuName, m) =>
        m match {
          case MenuValue(v) =>
            val li = st.li(st.a(st.href := "#", st.tabindex := -1)(menuName)).render
            li.onclick = { _ => onSelect(v) }
            li
          case MenuLink(str) =>
            st.li(st.a(st.href := str, st.tabindex := -1)(menuName))
          case n: MenuNode[T] =>
            st.li(st.cls := "dropdown-submenu")(
              st.a(st.href := "#", st.tabindex := -1)(menuName),
              dropDownMenu(n, onSelect)
            )
        }
      }.toSeq: _*
    )
    menu.render
  }

  def menuBar[T](menuItems: Seq[(List[String], MenuItem[T])], itemSelected: Sin[T]): BindableComp =
    dynamic { implicit po =>
      val menuTree = MenuTree.toTree(menuItems)
      dropDownMenu[T](menuTree, item => itemSelected.push(item))
    }

  def replaceable(property: Obs[BindableComp]): BindableComp =
    dynamic { implicit po =>
      ???
    }
}
