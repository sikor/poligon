package poligon.exampleapp.components

import com.vaadin.ui.Component
import poligon.exampleapp.components.BindableComp.{DynamicComp, StaticComp}
import poligon.polyproperty.HasSimplePropertyCodec
import poligon.polyproperty.PropertyObserver.PropertyObservers


sealed trait BindableComp {
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

object BindableComp extends HasSimplePropertyCodec[BindableComp] {


  class StaticComp(factory: => Component) extends BindableComp {
    def staticBind: Component = factory
  }

  trait DynamicComp extends BindableComp {
    def bindDynamic(po: PropertyObservers): Component
  }


  def dynamic(factory: PropertyObservers => Component): DynamicComp = (po: PropertyObservers) => factory(po)

  def static(factory: => Component): StaticComp = new StaticComp(factory)

  def factory(factory: => BindableComp): BindableComp = dynamic(po => factory.bind(po))
}
