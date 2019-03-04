package poligon.comp

import monix.eval.Task
import poligon.polyproperty.GAct
import poligon.polyproperty.PropertyObserver.GPropertyObservers


object BindableComp {

  type BindableComp[+T, -D] = GAct[T, GPropertyObservers[D]]

  def bind[C, D](bc: BindableComp[C, D], parentPo: GPropertyObservers[D]): Task[C] = {
    val po = parentPo.createSubObservers()
    val cTask = bc.run(po)
    cTask.map { c =>
      parentPo.registerSubObservers(c, po)
      c
    }
  }

  def dynamic[T, D](factory: GPropertyObservers[D] => Task[T]): BindableComp[T, D] =
    (po: GPropertyObservers[D]) => factory(po)

  def simple[T, D](factory: GPropertyObservers[D] => T): BindableComp[T, D] =
    (po: GPropertyObservers[D]) => Task.now(factory(po))

}
