package poligon

import monix.eval.Task
import org.scalatest.FunSuite
import org.scalatest.concurrent.ScalaFutures
import poligon.ReaderMonadTest.Reader

object ReaderMonadTest {

  trait Reader[+T, -C] {
    self =>
    def run(rootScope: C): Task[T]

    def map[T2](f: T => T2): Reader[T2, C] =
      Reader.create(rootScope => self.run(rootScope).map(f))

    def c[C2 <: C]: Reader[T, C2] = this
  }

  object Reader {
    def create[T, C](f: C => Task[T]): Reader[T, C] = (rootScope: C) => f(rootScope)

    implicit class ReaderExt[T, C](private val r: Reader[T, C]) extends AnyVal {
      def flatMap[T2](f: T => Reader[T2, C]): Reader[T2, C] =
        create(rootScope => r.run(rootScope).flatMap(v => f(v).run(rootScope)))
    }

  }

}

class ReaderMonadTest extends FunSuite with ScalaFutures {

  case class Context(s: String, myInt: Int) extends HasInt

  trait HasInt {
    def myInt: Int
  }

  def useReader(reader: Reader[Int, HasInt]): Unit = {
    reader.map(_ => ())
  }

  def useReader2[C <: HasInt](reader: Reader[Int, C]): Unit = {
    reader.map(_ => ())
  }

  test("contravariance") {
    val getInt = Reader.create((r: HasInt) => Task.now(r.myInt))
    val getString = Reader.create((c: Context) => Task.now(c.s))

    val task: Reader[String, Context] = for {
      i <- getInt.c[Context]
      s <- getString
    } yield i.toString + "_" + s

    val result = task.run(Context("aaa", 10)).runAsync(monix.execution.Scheduler.Implicits.global).futureValue

    useReader(getInt)
    useReader2(getInt.c[Context])
    assert(result == "10_aaa")
  }

}
