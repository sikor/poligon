package poligon.exampleapp.view

import com.typesafe.scalalogging.StrictLogging
import poligon.exampleapp.properties.{Binder, Comp}
import poligon.exampleapp.services.ExecuteTasksService
import poligon.exampleapp.view.ExecuteTasksButton.ExecuteTasksStatus.{InProgress, NotStarted}
import poligon.polyproperty.{HasSimplePropertyCodec, PropertyWithParent, Sin}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object ExecuteTasksButton {

  sealed trait ExecuteTasksStatus

  object ExecuteTasksStatus extends HasSimplePropertyCodec[ExecuteTasksStatus] {

    case object NotStarted extends ExecuteTasksStatus

    case object InProgress extends ExecuteTasksStatus

    case object Success extends ExecuteTasksStatus

    case object Failed extends ExecuteTasksStatus

  }

  class ExecuteTasksContext(service: ExecuteTasksService)(implicit ec: ExecutionContext) extends StrictLogging {
    val executeTaskStatus: PropertyWithParent[ExecuteTasksStatus] = PropertyWithParent(NotStarted)

    def executeTasks: Sin[Unit] = Sin(implicit po => _ => {
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
    })
  }

  def create(presenter: ExecuteTasksContext): Comp =
    Binder.button(
      presenter.executeTasks,
      presenter.executeTaskStatus.map(s => s.toString),
      presenter.executeTaskStatus.map(s => s != InProgress))
}
