package poligon.scalajscomp

import com.avsystem.commons.SharedExtensions.MapOps.Entry
import org.scalajs.dom.Element
import org.scalajs.dom.raw.Node
import poligon.comp.CompFamily
import poligon.comp.CompFamily.LayoutModification.{Added, Removed}
import poligon.comp.CompFamily.MenuTree.{MenuItem, MenuLink, MenuNode, MenuValue}
import poligon.comp.CompFamily._
import poligon.polyproperty.{Obs, Sin}
import scalatags.JsDom.all._
import scalatags.JsDom.{all => st}

object ScalaJsCompFamily extends CompFamily {

  type ComponentT = Element

  trait LayoutBuilder {
    def container: Element

    def addElement(index: Int, element: Element): Unit

    def removeElement(index: Int): Node
  }

  class VerticalLayoutBuilder extends LayoutBuilder {
    val container: Element = st.div(st.cls := "container").render

    def addElement(index: Int, element: Element): Unit = {
      val row = st.div(st.cls := "row")(
        st.div(st.cls := "col")(
          element
        )
      ).render
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
    private val row = st.div(st.cls := "row").render
    val container: Element = st.div(st.cls := "container")(
      row
    ).render

    def addElement(index: Int, element: Element): Unit = {
      val col = st.div(st.cls := "col")(
        element
      ).render
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
            val c = added.bind(po)
            builder.addElement(index, c)
        }
      }
      builder.container
    }

  def label(property: Obs[String], styleName: String): BindableComp = dynamic { implicit po =>
    val l = st.span(st.cls := styleName).render
    property.listen(s => l.innerHTML = s)
    l
  }

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): BindableComp =
    dynamic { implicit po =>
      val input = st.input(st.`type` := "text", st.cls := "form-control", st.id := caption).render
      input.onchange = { _ => onValueSet.push(input.value) }
      st.div(st.cls := "form-group")(
        st.label(st.`for` := caption)(caption),
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
      val in = st.input(st.`type` := "checkbox", st.value := caption).render
      in.checked = initValue
      in.onclick = { _ =>
        value.push(in.checked)
      }
      st.div(cls := "checkbox")(
        st.label(in, caption)
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
      val wrapper = st.div().render
      property.listen { c =>
        val currentContent = wrapper.firstChild
        val newContent = c.bind(po)
        po.deregisterSubObservers(currentContent)
        if (currentContent != null) {
          wrapper.replaceChild(newContent, currentContent)
        } else {
          wrapper.appendChild(newContent)
        }
      }
      wrapper
    }
}
