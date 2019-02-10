package poligon.scalajscomp

import org.scalajs.dom
import org.scalajs.dom.Element
import org.scalajs.dom.raw.Node
import poligon.comp.Comp.LayoutModification.{Added, Removed}
import poligon.comp.Comp.{LayoutModification, LayoutSettings}
import poligon.comp.CompFactory
import poligon.polyproperty.{Obs, Sin}

object ScalaJsCompFactory extends CompFactory {


  type ComponentT = Element

  def layout(
              property: Obs[Seq[LayoutModification[BindableComp]]],
              layoutDescription: LayoutSettings): BindableComp =
    dynamic { implicit po =>
      val container = dom.document.createElement("div")
      container.setAttribute("class", "container")
      property.listen { modifications =>
        modifications.foreach {
          case Removed(index) =>
            val removedComponent: Node = container.childNodes(index)
            po.deregisterSubObservers(removedComponent)
            container.removeChild(removedComponent)
          case Added(index, added) =>
            //            val c = added.looseBind(po)
            ???
        }
      }
      container
    }

  def label(property: Obs[String], styleName: String): BindableComp = ???

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): BindableComp = ???

  def button(onClick: Sin[Unit], caption: Obs[String], enabled: Obs[Boolean]): BindableComp = ???

  def checkBox(caption: String, initValue: Boolean, value: Sin[Boolean]): BindableComp = ???

  def menuBar[T](menuItems: Seq[(List[String], T)], itemSelected: Sin[T]): BindableComp = ???

  def replaceable(property: Obs[BindableComp]): BindableComp = ???
}
