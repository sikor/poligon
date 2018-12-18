package databinding.properties

import com.vaadin.ui.Component
import databinding.properties.Comp.{DynamicComp, StaticComp}
import poligon.polyproperty.PropertyObserver.PropertyObservers

sealed trait Comp {
  def bind(poFactory: => PropertyObservers): Component = {
    this match {
      case s: StaticComp => s.staticBind
      case d: DynamicComp =>
        val po = poFactory
        new DynamicComponent(d.bindDynamic(po), po)
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

  def unbind(component: Component): Unit = DynamicComponent.dispose(component)
}
