package poligon.polyproperty

import monix.eval.Task

trait GObs[+T, -S] {
  self =>

  def listen(listener: T => Task[Unit], scope: S): Task[Unit]

  def listenNow(listener: T => Unit, scope: S): Task[Unit] =
    listen(v => Task.now(listener(v)), scope)

  def c[S2 <: S]: GObs[T, S2] = this
}


object GObs {

  implicit class GObsExt[T, S](private val g: GObs[T, S]) extends AnyVal {
    def foreach(f: T => GAct[Unit, S]): GAct[Unit, S] =
      GAct.create(s => g.listen(v => f(v).run(s), s))

    def map[R](f: T => R): GObs[R, S] = mapAsync(v => GAct.now(f(v)))

    def mapAsync[R](f: T => GAct[R, S]): GObs[R, S] = new MapGObs[T, R, S](g, f)

  }

  def create[T, S](f: (T => Task[Unit], S) => Task[Unit]): GObs[T, S] =
    (listener: T => Task[Unit], scope: S) => f(listener, scope)

  def constant[T](value: T): GObs[T, Any] = create((f, _) => f(value))

  private class MapGObs[T, T2, S](source: GObs[T, S], map: T => GAct[T2, S]) extends GObs[T2, S] {
    def listen(listener: T2 => Task[Unit], scope: S): Task[Unit] = {
      source.listen(v => map(v).flatMap(fv => GAct.fromTask(listener(fv))).run(scope), scope)
    }
  }

}