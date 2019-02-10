package poligon.exampleapp.services

import java.util.concurrent.{Executors, ThreadLocalRandom, TimeUnit}

import scala.concurrent.{Future, Promise}

class ExecuteTasksService {

  private val executor = Executors.newScheduledThreadPool(1)

  def executeTasks(): Future[Boolean] = {
    val p = Promise[Boolean]
    executor.schedule(() => {
      p.success(ThreadLocalRandom.current().nextBoolean())
    }, 5, TimeUnit.SECONDS)
    p.future
  }

}
