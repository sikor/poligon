package poligon
package polyproperty

import monix.eval.Task
import monix.reactive.Observable
import poligon.polyproperty.Act.{BAct, GSin}
import poligon.polyproperty.Obs.BObs
import poligon.polyproperty.PropertyObserver.{AnyPropertyObservers, GPropertyObservers}

trait BaseAct[D] {

  type Sin[T] = GSin[T, D]
  type Act[+T] = BAct[T, D]
  type Obs[+T] = BObs[T, D]

  def create[T](f: GPropertyObservers[D] => Task[T]): Act[T] = (scope: GPropertyObservers[D]) => f(scope)

  def deps[T](f: D => Task[T]): Act[T] = create(s => f(s.deps))

  def depsNow[T](f: D => T): Act[T] = create(s => Task.now(f(s.deps)))

  def depsFlat[T](f: D => Act[T]): Act[T] = create(s => f(s.deps).run(s))

  def createObs[T](f: D => Observable[T]): Obs[T] = GObs.create { (listener, scope) =>
    scope.subscribeToObservable(f(scope.deps), listener)
    Task.unit
  }
}

object Act {

  type AnyAct[+T] = GAct[T, AnyPropertyObservers]
  type BAct[+T, -D] = GAct[T, GPropertyObservers[D]]

  type GSin[T, -D] = T => BAct[Unit, D]

  def push[T, D](v: T, f: T => BAct[Unit, D])(implicit po: GPropertyObservers[D]): Unit = {
    po.taskRunner.handleEvent(v, f, po)
  }

  def create[T](f: AnyPropertyObservers => Task[T]): AnyAct[T] = (scope: AnyPropertyObservers) => f(scope)

  def fromTask[T](task: Task[T]): AnyAct[T] = create(_ => task)

  def defer[T](act: => AnyAct[T]): AnyAct[T] = create(r => act.run(r))

  val unit: AnyAct[Unit] = Act.create(_ => Task.unit)

}
