package poligon.comp

import poligon.polyproperty.PropertyObserver.PropertyObservers


class BindableComp[C](val create: PropertyObservers => C) {
  def bind(parentPo: PropertyObservers): C = {
    val po = parentPo.createSubObservers()
    val c = create(po)
    parentPo.registerSubObservers(c, po)
    c
  }
}

object BindableComp {

  def dynamic[C](factory: PropertyObservers => C): BindableComp[C] =
    new BindableComp((po: PropertyObservers) => factory(po))

}
