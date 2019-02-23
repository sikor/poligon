package poligon.comp

import poligon.polyproperty.PropertyObserver.PropertyObservers


trait HasBindableComp {

  type ComponentT

  class BindableComp(val create: PropertyObservers => ComponentT) {
    def bind(parentPo: PropertyObservers): ComponentT = {
      val po = parentPo.createSubObservers()
      val c = create(po)
      parentPo.registerSubObservers(c, po)
      c
    }
  }

  def dynamic(factory: PropertyObservers => ComponentT): BindableComp =
    new BindableComp((po: PropertyObservers) => factory(po))

}
