package poligon.scalajscomp

import org.scalajs.dom
import org.scalajs.dom.Element
import org.scalajs.dom.raw.Node
import poligon.comp.Comp.LayoutModification.{Added, Removed}
import poligon.comp.Comp.{Form, Horizontal, LayoutModification, LayoutSettings, Vertical}
import poligon.comp.CompFactory
import poligon.polyproperty.{Obs, Sin}

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

  def label(property: Obs[String], styleName: String): BindableComp = ???

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): BindableComp = ???

  def button(onClick: Sin[Unit], caption: Obs[String], enabled: Obs[Boolean]): BindableComp = ???

  def checkBox(caption: String, initValue: Boolean, value: Sin[Boolean]): BindableComp = ???

  def menuBar[T](menuItems: Seq[(List[String], T)], itemSelected: Sin[T]): BindableComp = ???

  def replaceable(property: Obs[BindableComp]): BindableComp = ???
}
