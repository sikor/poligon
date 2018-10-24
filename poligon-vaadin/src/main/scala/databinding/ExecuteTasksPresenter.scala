package databinding

import com.typesafe.scalalogging.StrictLogging
import databinding.ExecuteTasksPresenter.ExecuteTasksStatus
import databinding.ExecuteTasksPresenter.ExecuteTasksStatus.{InProgress, NotStarted}
import io.udash.properties.single.Property

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}


object ExecuteTasksPresenter {

  sealed trait ExecuteTasksStatus

  object ExecuteTasksStatus {

    case object NotStarted extends ExecuteTasksStatus

    case object InProgress extends ExecuteTasksStatus

    case object Success extends ExecuteTasksStatus

    case object Failed extends ExecuteTasksStatus

  }

}

class ExecuteTasksPresenter(service: ExecuteTasksService)(implicit ec: ExecutionContext) extends Presenter with StrictLogging {
  val model: Property[ExecuteTasksStatus] = Property(NotStarted)

  def executeTasks(): Unit = {
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
