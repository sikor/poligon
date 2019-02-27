package poligon.polyproperty

import monix.eval.Task
import poligon.polyproperty.PropertyObserver.{PropertyObservers, RootPropertyObservers}

trait Act[T] {
  self =>

  def run(rootScope: RootPropertyObservers): Task[T]

  def flatMap[T2](f: T => Act[T2]): Act[T2] =
    Act.create(rootScope => self.run(rootScope).flatMap(v => f(v).run(rootScope)))

  def map[T2](f: T => T2): Act[T2] =
    Act.create(rootScope => self.run(rootScope).map(f))
}

object Act {

  type Sin[T] = T => Act[Unit]

  def push[T](v: T, f: T => Act[Unit])(implicit po: PropertyObservers): Unit = {
    po.taskRunner.handleEvent(v, f, po)
  }

  def create[T](f: RootPropertyObservers => Task[T]): Act[T] = (rootScope: RootPropertyObservers) => f(rootScope)

  def fromTask[T](task: Task[T]): Act[T] = create(_ => task)

  def defer[T](act: => Act[T]): Act[T] = create(r => act.run(r))

  val unit: Act[Unit] = Act.create(_ => Task.unit)

}
