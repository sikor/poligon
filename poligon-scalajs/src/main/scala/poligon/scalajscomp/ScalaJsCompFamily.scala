package poligon.scalajscomp

import com.avsystem.commons.SharedExtensions.MapOps.Entry
import monix.eval.Task
import org.scalajs.dom.Element
import org.scalajs.dom.raw.Node
import poligon.comp.BindableComp.BindableComp
import poligon.comp.CompFamily.LayoutModification.{Added, Removed}
import poligon.comp.CompFamily.MenuTree.{MenuItem, MenuLink, MenuNode, MenuValue}
import poligon.comp.CompFamily._
import poligon.comp.{BindableComp, CompFamily}
import poligon.polyproperty.Act
import poligon.polyproperty.Act.GSin
import poligon.polyproperty.Obs.BObs
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

  def layout[D](
                 property: BObs[Seq[LayoutModification[BindableComp[Element, D]]], D],
                 layoutDescription: LayoutSettings): BindableComp[Element, D] =
    dynamic { po =>
      val builder = layoutDescription.layoutType match {
        case Horizontal =>
          new HorizontalLayoutBuilder
        case Vertical | Form =>
          new VerticalLayoutBuilder
      }
      property.listenOn(po) { modifications =>
        gatherModifications(modifications, po).map(_.foreach {
          case Removed(index) =>
            val removedComponent: Node = builder.removeElement(index)
            po.deregisterSubObservers(removedComponent)
          case Added(index, added) =>
            builder.addElement(index, added)
        })
      }.map(_ => builder.container)
    }

  def label[D](property: BObs[String, D], styleName: String): BindableComp[Element, D] = dynamic { po =>
    val l = st.span(st.cls := styleName).render
    property.listenNow(po)(s => l.innerHTML = s).map(_ => l)
  }

  def textField[D](caption: String, initValue: String, onValueSet: GSin[String, D]): BindableComp[Element, D] =
    simple { implicit po =>
      val input = st.input(st.`type` := "text", st.cls := "form-control", st.id := caption).render
      input.onchange = { _ => Act.push(input.value, onValueSet) }
      st.div(st.cls := "form-group")(
        st.label(st.`for` := caption)(caption),
        input
      ).render
    }

  def button[D](onClick: GSin[Unit, D], caption: BObs[String, D], enabled: BObs[Boolean, D]): BindableComp[Element, D] =
    dynamic { implicit po =>
      val b = st.button(st.cls := "btn").render
      b.onclick = { _ => Act.push((), onClick) }
      val t1 = caption.listenNow(po)(v => b.innerHTML = v)
      val t2 = enabled.listenNow(po)(enabled => b.disabled = !enabled)
      Task.gatherUnordered(List(t1, t2)).map(_ => b)
    }

  def checkBox[D](caption: String, initValue: Boolean, value: GSin[Boolean, D]): BindableComp[Element, D] =
    simple { po =>
      val in = st.input(st.`type` := "checkbox", st.value := caption).render
      in.checked = initValue
      in.onclick = { _ =>
        Act.push(in.checked, value)(po)
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

  def menuBar[T, D](menuItems: Seq[(List[String], MenuItem[T])], itemSelected: GSin[T, D]): BindableComp[Element, D] =
    simple { implicit po =>
      val menuTree = MenuTree.toTree(menuItems)
      dropDownMenu[T](menuTree, item => Act.push(item, itemSelected))
    }

  def replaceable[D](child: BObs[BindableComp[Element, D], D]): BindableComp[Element, D] =
    dynamic { po =>
      val wrapper = st.div().render
      child.listenOn(po) { c =>
        BindableComp.bind(c, po).map { newContent =>
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
