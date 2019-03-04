package poligon.comp

import monix.eval.Task
import poligon.polyproperty.GAct
import poligon.polyproperty.PropertyObserver.PropertyObservers


object BindableComp {

  type BindableComp[+C] = GAct[C, PropertyObservers]

  def bind[C](bc: BindableComp[C], parentPo: PropertyObservers): Task[C] = {
    val po = parentPo.createSubObservers()
    val cTask = bc.run(po)
    cTask.map { c =>
      parentPo.registerSubObservers(c, po)
      c
    }
  }

  def dynamic[C](factory: PropertyObservers => Task[C]): BindableComp[C] =
    (po: PropertyObservers) => factory(po)

  def simple[C](factory: PropertyObservers => C): BindableComp[C] =
    (po: PropertyObservers) => Task.now(factory(po))

}
