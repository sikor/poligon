package poligon.exampleapp.properties

import com.vaadin.ui.Component
import poligon.exampleapp.properties.Comp.{DynamicComp, StaticComp}
import poligon.polyproperty.PropertyObserver.PropertyObservers


sealed trait Comp {
  def looseBind(parentPo: PropertyObservers): Component = {
    this match {
      case s: StaticComp => s.staticBind
      case d: DynamicComp =>
        val po = parentPo.createSubObservers()
        val c = d.bindDynamic(po)
        parentPo.registerSubObservers(c, po)
        c
    }
  }

  def bind(parentPo: PropertyObservers): Component = {
    this match {
      case s: StaticComp => s.staticBind
      case d: DynamicComp => d.bindDynamic(parentPo)
    }
  }
}

object Comp {


  class StaticComp(factory: => Component) extends Comp {
    def staticBind: Component = factory
  }

  trait DynamicComp extends Comp {
    def bindDynamic(po: PropertyObservers): Component
  }


  def dynamic(factory: PropertyObservers => Component): DynamicComp = (po: PropertyObservers) => factory(po)

  def static(factory: => Component): StaticComp = new StaticComp(factory)

}
