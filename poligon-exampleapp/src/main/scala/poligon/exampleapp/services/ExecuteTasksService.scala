package poligon.exampleapp.services

import java.util.concurrent.ThreadLocalRandom

import monix.eval.Task

import scala.concurrent.duration._

class ExecuteTasksService {

  def executeTasks(): Task[Boolean] = {
    Task.eval(ThreadLocalRandom.current().nextBoolean()).delayExecution(5.seconds)
  }

}
