package exampleapp.view

import com.typesafe.scalalogging.StrictLogging
import exampleapp.view.ExecuteTasksPresenter.ExecuteTasksStatus
import exampleapp.view.ExecuteTasksPresenter.ExecuteTasksStatus.{InProgress, NotStarted}
import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty.{HasSimplePropertyCodec, Property, PropertyWithParent}

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
  private val model: PropertyWithParent[ExecuteTasksStatus] = PropertyWithParent(NotStarted)

  def getModel: Property[ExecuteTasksStatus] = model.property

  def executeTasks(implicit observed: PropertyObservers): Unit = {
    model.set(InProgress)
    service.executeTasks().onComplete {
      case Success(b) =>
        logger.info(s"Tasks executed success: $b")
        if (b) {
          model.set(ExecuteTasksStatus.Success)
        } else {
          model.set(ExecuteTasksStatus.Failed)
        }
      case Failure(ex) =>
        logger.error("et failed", ex)
        model.set(ExecuteTasksStatus.Failed)
    }
  }


}
