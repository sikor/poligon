package poligon.polyproperty

import monix.eval.Task
import poligon.polyproperty.PropertyObserver.{PropertyObservers, RootPropertyObservers}

object Act {

  type Act[T] = GAct[T, RootPropertyObservers]

  type Sin[T] = T => Act[Unit]

  def push[T](v: T, f: T => Act[Unit])(implicit po: PropertyObservers): Unit = {
    po.taskRunner.handleEvent(v, f, po)
  }

  def create[T](f: RootPropertyObservers => Task[T]): Act[T] = (rootScope: RootPropertyObservers) => f(rootScope)

  def fromTask[T](task: Task[T]): Act[T] = create(_ => task)

  def defer[T](act: => Act[T]): Act[T] = create(r => act.run(r))

  val unit: Act[Unit] = Act.create(_ => Task.unit)

}
