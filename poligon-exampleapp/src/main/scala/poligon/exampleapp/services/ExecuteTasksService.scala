package poligon.exampleapp.services

import java.util.concurrent.ThreadLocalRandom

import monix.execution.Scheduler

import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

class ExecuteTasksService(scheduler: Scheduler) {

  def executeTasks(): Future[Boolean] = {
    val p = Promise[Boolean]
    scheduler.scheduleOnce(5.seconds) {
      p.success(ThreadLocalRandom.current().nextBoolean())
    }
    p.future
  }

}
