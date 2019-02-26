package poligon.polyproperty

import poligon.polyproperty.PropertyObserver.RootPropertyObservers

trait Sin[T] {
  def push(v: T)(implicit po: RootPropertyObservers): Unit
}

object Sin {

  def eval[T](f: T => Act[Unit]): Sin[T] = new ActFunSin[T](f)

  class ActFunSin[T](f: T => Act[Unit]) extends Sin[T] {
    def push(v: T)(implicit po: RootPropertyObservers): Unit = {
      val task = f(v).run(po)
      po.taskRunner.runTask(task)
    }
  }

}
