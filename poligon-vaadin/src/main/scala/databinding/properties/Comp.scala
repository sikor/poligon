package databinding.properties

import com.vaadin.ui.Component
import databinding.properties.Comp.{Bound, DynamicComp, StaticComp}
import poligon.polyproperty.PropertyObserver.PropertyObservers

sealed trait Comp[T] {
  def looseBind(parentPo: PropertyObservers): Bound[T] = {
    this match {
      case s: StaticComp[T] => s.staticBind
      case d: DynamicComp[T] =>
        val po = parentPo.createSubObservers()
        val c = d.bindDynamic(po)
        parentPo.registerSubObservers(c, po)
        c
    }
  }

  def bind(parentPo: PropertyObservers): Bound[T] = {
    this match {
      case s: StaticComp[T] => s.staticBind
      case d: DynamicComp[T] => d.bindDynamic(parentPo)
    }
  }


}

object Comp {

  class Bound[T](supplier: () => T, val comp: Component) {
    def get: T = supplier()
  }

  def unitBound(component: Component): Bound[Unit] = new Bound[Unit](() => (), component)

  def bound[T](value: => T, component: Component): Bound[T] = new Bound[T](() => value, component)

  class StaticComp[T](factory: => Bound[T]) extends Comp[T] {
    def staticBind: Bound[T] = factory
  }

  trait DynamicComp[T] extends Comp[T] {
    def bindDynamic(po: PropertyObservers): Bound[T]
  }

  def dynamicUnit(factory: PropertyObservers => Component): DynamicComp[Unit] = dynamic(po => unitBound(factory(po)))

  def dynamic[T](factory: PropertyObservers => Bound[T]): DynamicComp[T] = (po: PropertyObservers) => factory(po)

  def static[T](factory: => Bound[T]): StaticComp[T] = new StaticComp(factory)

  def staticUnit(factory: => Component): StaticComp[Unit] = new StaticComp(unitBound(factory))

}
