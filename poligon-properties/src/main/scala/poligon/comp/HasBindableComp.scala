package poligon.comp

import poligon.polyproperty.PropertyObserver.PropertyObservers


trait HasBindableComp {

  type ComponentT

  sealed trait BindableComp {

    def looseBind(parentPo: PropertyObservers): ComponentT = {
      this match {
        case s: StaticComp => s.staticBind
        case d: DynamicComp =>
          val po = parentPo.createSubObservers()
          val c = d.bindDynamic(po)
          parentPo.registerSubObservers(c, po)
          c
      }
    }

    def bind(parentPo: PropertyObservers): ComponentT = {
      this match {
        case s: StaticComp => s.staticBind
        case d: DynamicComp => d.bindDynamic(parentPo)
      }
    }
  }


  class StaticComp(factory: => ComponentT) extends BindableComp {
    def staticBind: ComponentT = factory
  }

  trait DynamicComp extends BindableComp {
    def bindDynamic(po: PropertyObservers): ComponentT
  }


  def dynamic(factory: PropertyObservers => ComponentT): DynamicComp = (po: PropertyObservers) => factory(po)

  def static(factory: => ComponentT): StaticComp = new StaticComp(factory)

  def factory(factory: => BindableComp): BindableComp = dynamic(po => factory.bind(po))
}
