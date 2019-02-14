package poligon.exampleapp.services

import java.util.concurrent.ThreadLocalRandom

import monix.execution.Scheduler

import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

class ExecuteTasksService {

  def executeTasks(): Future[Boolean] = {
    val p = Promise[Boolean]
    Scheduler.Implicits.global.scheduleOnce(5.seconds)(() =>
      p.success(ThreadLocalRandom.current().nextBoolean())
    )
    p.future
  }

}
