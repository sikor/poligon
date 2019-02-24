package poligon

import monix.eval.Task
import org.scalatest.FunSuite
import org.scalatest.concurrent.ScalaFutures

class MonixTaskTest extends FunSuite with ScalaFutures {

  test("memoize") {
    val printer: Task[Int] = Task.eval {
      println("printer")
      1
    }

    def printerUsers = {
      val p = printer.memoize
      Range(0, 10).map(i => p.map(v => v + i))
    }

    def printAll = Task.sequence(printerUsers)

    val scheduler = monix.execution.Scheduler.Implicits.global
    printAll.runAsync(scheduler).futureValue
    printAll.runAsync(scheduler).futureValue
  }

  test("run sync") {
    val printer: Task[Int] = Task.eval {
      println("printer")
      1
    }

    def printerUsers = {
      val p = printer.memoize
      Range(0, 10).map(i => p.map(v => v + i))
    }

    def printAll = Task.sequence(printerUsers)

    val scheduler = monix.execution.Scheduler.Implicits.global
    printAll.runSyncMaybe(scheduler) match {
      case Left(f) => println("async: " + f.futureValue)
      case Right(v) => println("sync: " + v)
    }
  }

}
