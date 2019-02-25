package poligon.polyproperty

import poligon.polyproperty.PropertyObserver.RootPropertyObservers
import poligon.polyproperty.Sin.MapSin

trait Sin[T] {
  def rMap[R](f: R => T): Sin[R] = new MapSin[T, R](this, v => f(v))

  def push(v: T)(implicit po: RootPropertyObservers): Unit
}

object Sin {

  def apply[T](f: RootPropertyObservers => T => Unit): Sin[T] = new FunSin[T](f)

  def apply[T](sub: Sin[T]*): Sin[T] = new Sin[T] {
    def push(v: T)(implicit po: RootPropertyObservers): Unit = sub.foreach(_.push(v))
  }

  def combine[T](sub: Sin[T]*): Sin[T] = apply(sub: _*)

  def eval[T](f: T => Unit): Sin[T] = new FunSin[T](_ => v => f(v))

  def evalAsync[T](f: T => Act[Unit]): Sin[T] = new ActFunSin[T](f)

  class ActFunSin[T](f: T => Act[Unit]) extends Sin[T] {
    def push(v: T)(implicit po: RootPropertyObservers): Unit = {
      val task = f(v).run(po)
      po.taskRunner.runTask(task)
    }
  }

  class FunSin[T](f: RootPropertyObservers => T => Unit) extends Sin[T] {
    def push(v: T)(implicit po: RootPropertyObservers): Unit = f(po)(v)
  }

  class MapSin[S, T](child: Sin[S], map: T => S) extends Sin[T] {
    def push(v: T)(implicit po: RootPropertyObservers): Unit = child.push(map(v))
  }

}
