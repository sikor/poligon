package poligon.comp

import poligon.polyproperty.PropertyObserver.PropertyObservers


trait CompT {

  type ComponentT

  sealed trait Comp {

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


  class StaticComp(factory: => ComponentT) extends Comp {
    def staticBind: ComponentT = factory
  }

  trait DynamicComp extends Comp {
    def bindDynamic(po: PropertyObservers): ComponentT
  }


  def dynamic(factory: PropertyObservers => ComponentT): DynamicComp = (po: PropertyObservers) => factory(po)

  def static(factory: => ComponentT): StaticComp = new StaticComp(factory)

  def factory(factory: => Comp): Comp = dynamic(po => factory.bind(po))
}
