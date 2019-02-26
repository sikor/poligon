package poligon.polyproperty

import poligon.polyproperty.PropertyObserver.PropertyObservers

trait Sin[T] {
  def push(v: T)(implicit po: PropertyObservers): Unit
}

object Sin {

  def eval[T](f: T => Act[Unit]): Sin[T] = new ActFunSin[T](f)
  def evalUnit(f: => Act[Unit]): Sin[Unit] = new ActFunSin[Unit](_ => f)

  class ActFunSin[T](f: T => Act[Unit]) extends Sin[T] {
    def push(v: T)(implicit po: PropertyObservers): Unit = {
      po.taskRunner.handleEvent(v, f, po)
    }
  }

}
