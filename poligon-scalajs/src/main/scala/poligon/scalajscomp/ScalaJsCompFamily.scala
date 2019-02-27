package poligon.scalajscomp

import com.avsystem.commons.SharedExtensions.MapOps.Entry
import monix.eval.Task
import org.scalajs.dom.Element
import org.scalajs.dom.raw.Node
import poligon.comp.CompFamily
import poligon.comp.CompFamily.LayoutModification.{Added, Removed}
import poligon.comp.CompFamily.MenuTree.{MenuItem, MenuLink, MenuNode, MenuValue}
import poligon.comp.CompFamily._
import poligon.polyproperty.Act.Sin
import poligon.polyproperty.{Act, Obs}
import scalatags.JsDom.all._
import scalatags.JsDom.{all => st}

object ScalaJsCompFamily extends CompFamily[Element] {

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
              property: Obs[Seq[LayoutModification[BComp]]],
              layoutDescription: LayoutSettings): BComp =
    dynamic { implicit po =>
      val builder = layoutDescription.layoutType match {
        case Horizontal =>
          new HorizontalLayoutBuilder
        case Vertical | Form =>
          new VerticalLayoutBuilder
      }
      property.listen { modifications =>
        gatherModifications(modifications).map(_.foreach {
          case Removed(index) =>
            val removedComponent: Node = builder.removeElement(index)
            po.deregisterSubObservers(removedComponent)
          case Added(index, added) =>
            builder.addElement(index, added)
        })
      }.map(_ => builder.container)
    }

  def label(property: Obs[String], styleName: String): BComp = dynamic { implicit po =>
    val l = st.span(st.cls := styleName).render
    property.listenNow(s => l.innerHTML = s).map(_ => l)
  }

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): BComp =
    simple { implicit po =>
      val input = st.input(st.`type` := "text", st.cls := "form-control", st.id := caption).render
      input.onchange = { _ => Act.push(input.value, onValueSet) }
      st.div(st.cls := "form-group")(
        st.label(st.`for` := caption)(caption),
        input
      ).render
    }

  def button(onClick: Sin[Unit], caption: Obs[String], enabled: Obs[Boolean]): BComp =
    dynamic { implicit po =>
      val b = st.button(st.cls := "btn").render
      b.onclick = { _ => Act.push((), onClick) }
      val t1 = caption.listenNow(v => b.innerHTML = v)
      val t2 = enabled.listenNow(enabled => b.disabled = !enabled)
      Task.gatherUnordered(List(t1, t2)).map(_ => b)
    }

  def checkBox(caption: String, initValue: Boolean, value: Sin[Boolean]): BComp =
    simple { implicit po =>
      val in = st.input(st.`type` := "checkbox", st.value := caption).render
      in.checked = initValue
      in.onclick = { _ =>
        Act.push(in.checked, value)
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

  def menuBar[T](menuItems: Seq[(List[String], MenuItem[T])], itemSelected: Sin[T]): BComp =
    simple { implicit po =>
      val menuTree = MenuTree.toTree(menuItems)
      dropDownMenu[T](menuTree, item => Act.push(item, itemSelected))
    }

  def replaceable(child: Obs[BComp]): BComp =
    dynamic { implicit po =>
      val wrapper = st.div().render
      child.listen { c =>
        c.bind(po).map { newContent =>
          val currentContent = wrapper.firstChild
          po.deregisterSubObservers(currentContent)
          if (currentContent != null) {
            wrapper.replaceChild(newContent, currentContent)
          } else {
            wrapper.appendChild(newContent)
          }
        }
      }.map(_ => wrapper)
    }
}
