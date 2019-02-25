package poligon.comp

import monix.eval.Task
import poligon.polyproperty.PropertyObserver.PropertyObservers


trait BindableComp[C] {
  def create(po: PropertyObservers): Task[C]

  def bind(parentPo: PropertyObservers): Task[C] = {
    val po = parentPo.createSubObservers()
    val cTask = create(po)
    cTask.map { c =>
      parentPo.registerSubObservers(c, po)
      c
    }
  }
}

object BindableComp {

  def dynamic[C](factory: PropertyObservers => Task[C]): BindableComp[C] =
    (po: PropertyObservers) => factory(po)

  def simple[C](factory: PropertyObservers => C): BindableComp[C] =
    (po: PropertyObservers) => Task.now(factory(po))

}
