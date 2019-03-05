package poligon.polyproperty

import monix.eval.Task
import poligon.polyproperty.PropertyObserver.{AnyPropertyObservers, GPropertyObservers, PropertyObservers}

object Act {

  type AnyAct[+T] = GAct[T, AnyPropertyObservers]
  type Act[+T, -D] = GAct[T, GPropertyObservers[D]]

  type Sin[T] = T => AnyAct[Unit]

  def push[T](v: T, f: T => AnyAct[Unit])(implicit po: PropertyObservers): Unit = {
    po.taskRunner.handleEvent(v, f, po)
  }

  def create[T](f: AnyPropertyObservers => Task[T]): AnyAct[T] = (scope: AnyPropertyObservers) => f(scope)

  def fromTask[T](task: Task[T]): AnyAct[T] = create(_ => task)

  def defer[T](act: => AnyAct[T]): AnyAct[T] = create(r => act.run(r))

  val unit: AnyAct[Unit] = Act.create(_ => Task.unit)

}
