package poligon.exampleapp.view

import poligon.exampleapp.EAComp._
import poligon.exampleapp.EAComp.Comp
import poligon.exampleapp.services.ExecuteTasksService
import poligon.exampleapp.view.ExecuteTasksButton.ExecuteTasksStatus.{InProgress, NotStarted}
import poligon.polyproperty.Act.Sin
import poligon.polyproperty.{Act, HasSimplePropertyCodec, PropertyWithParent}

object ExecuteTasksButton {

  sealed trait ExecuteTasksStatus

  object ExecuteTasksStatus extends HasSimplePropertyCodec[ExecuteTasksStatus] {

    case object NotStarted extends ExecuteTasksStatus

    case object InProgress extends ExecuteTasksStatus

    case object Success extends ExecuteTasksStatus

    case object Failed extends ExecuteTasksStatus

  }

  class ExecuteTasksContext(service: ExecuteTasksService) {
    val executeTaskStatus: PropertyWithParent[ExecuteTasksStatus] = PropertyWithParent(NotStarted)

    def executeTasks: Sin[Unit] = _ =>
      for {
        _ <- executeTaskStatus.set(InProgress)
        isSuccess <- Act.fromTask(service.executeTasks())
        _ <- if (isSuccess) {
          executeTaskStatus.set(ExecuteTasksStatus.Success)
        } else {
          executeTaskStatus.set(ExecuteTasksStatus.Failed)
        }
      } yield ()
  }

  def create(ctx: ExecuteTasksContext): Comp =
    button(
      ctx.executeTasks,
      ctx.executeTaskStatus.map(s => s.toString),
      ctx.executeTaskStatus.map(s => s != InProgress))
}
