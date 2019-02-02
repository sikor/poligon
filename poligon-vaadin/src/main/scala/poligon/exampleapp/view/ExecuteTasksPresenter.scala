package poligon.exampleapp.view

import com.typesafe.scalalogging.StrictLogging
import poligon.exampleapp.services.ExecuteTasksService
import poligon.exampleapp.view.ExecuteTasksPresenter.ExecuteTasksStatus
import poligon.exampleapp.view.ExecuteTasksPresenter.ExecuteTasksStatus.{InProgress, NotStarted}
import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty.{HasSimplePropertyCodec, PropertyWithParent}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}


object ExecuteTasksPresenter {

  sealed trait ExecuteTasksStatus

  object ExecuteTasksStatus extends HasSimplePropertyCodec[ExecuteTasksStatus] {

    case object NotStarted extends ExecuteTasksStatus

    case object InProgress extends ExecuteTasksStatus

    case object Success extends ExecuteTasksStatus

    case object Failed extends ExecuteTasksStatus

  }

}

class ExecuteTasksPresenter(service: ExecuteTasksService)(implicit ec: ExecutionContext) extends StrictLogging {
  val executeTaskStatus: PropertyWithParent[ExecuteTasksStatus] = PropertyWithParent(NotStarted)

  def executeTasks(implicit observed: PropertyObservers): Unit = {
    executeTaskStatus.set(InProgress)
    service.executeTasks().onComplete {
      case Success(b) =>
        logger.info(s"Tasks executed success: $b")
        if (b) {
          executeTaskStatus.set(ExecuteTasksStatus.Success)
        } else {
          executeTaskStatus.set(ExecuteTasksStatus.Failed)
        }
      case Failure(ex) =>
        logger.error("et failed", ex)
        executeTaskStatus.set(ExecuteTasksStatus.Failed)
    }
  }


}
