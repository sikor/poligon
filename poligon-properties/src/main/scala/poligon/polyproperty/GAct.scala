package poligon.polyproperty

import monix.eval.Task

trait GAct[+T, -S] {
  self =>

  def run(rootScope: S): Task[T]

  def map[T2](f: T => T2): GAct[T2, S] =
    GAct.create(rootScope => self.run(rootScope).map(f))

  def c[S2 <: S]: GAct[T, S2] = this
}


object GAct {

  implicit class GActExt[T, S](private val g: GAct[T, S]) extends AnyVal {
    def flatMap[T2](f: T => GAct[T2, S]): GAct[T2, S] =
      create(rootScope => g.run(rootScope).flatMap(v => f(v).run(rootScope)))
  }

  def now[T](v: T): GAct[T, Any] = create(_ => Task.now(v))

  def create[T, S](f: S => Task[T]): GAct[T, S] = (rootScope: S) => f(rootScope)

  def fromTask[T, S](task: Task[T]): GAct[T, S] = create(_ => task)

  def defer[T, S](act: => GAct[T, S]): GAct[T, S] = create(r => act.run(r))

  val unit: GAct[Unit, Any] = create(_ => Task.unit)

}
