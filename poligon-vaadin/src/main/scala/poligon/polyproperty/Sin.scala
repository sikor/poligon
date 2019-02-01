package poligon.polyproperty

import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty.Sin.MapSin

trait Sin[T] {
  def rMap[R](f: R => T): Sin[R] = new MapSin[T, R](this, v => f(v))


  def push(v: T)(implicit po: PropertyObservers): Unit
}

object Sin {

  def apply[T](f: PropertyObservers => T => Unit): Sin[T] = new FunSin[T](f)

  def apply[T](sub: Sin[T]*): Sin[T] = new Sin[T] {
    def push(v: T)(implicit po: PropertyObservers): Unit = sub.foreach(_.push(v))
  }

  class FunSin[T](f: PropertyObservers => T => Unit) extends Sin[T] {
    def push(v: T)(implicit po: PropertyObservers): Unit = f(po)(v)
  }

  class MapSin[S, T](child: Sin[S], map: T => S) extends Sin[T] {
    def push(v: T)(implicit po: PropertyObservers): Unit = child.push(map(v))
  }


}
