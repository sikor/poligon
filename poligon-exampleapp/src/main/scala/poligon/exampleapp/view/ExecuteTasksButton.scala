package poligon.exampleapp.view

import poligon.comp.Comp
import Comp._
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

  class ExecuteTasksContext(service: ExecuteTasksService)(implicit ec: ExecutionContext) {
    val executeTaskStatus: PropertyWithParent[ExecuteTasksStatus] = PropertyWithParent(NotStarted)

    def executeTasks2: Sin[Unit] = Sin.eval { _ =>
//      executeTaskStatus.set()
    }

    def executeTasks: Sin[Unit] = Sin(implicit po => _ => {
      executeTaskStatus.set(InProgress)
      service.executeTasks().onComplete {
        case Success(b) =>
          if (b) {
            executeTaskStatus.set(ExecuteTasksStatus.Success)
          } else {
            executeTaskStatus.set(ExecuteTasksStatus.Failed)
          }
        case Failure(ex) =>
          executeTaskStatus.set(ExecuteTasksStatus.Failed)
      }
    })
  }

  def create(presenter: ExecuteTasksContext): Comp =
    button(
      presenter.executeTasks,
      presenter.executeTaskStatus.map(s => s.toString),
      presenter.executeTaskStatus.map(s => s != InProgress))
}
