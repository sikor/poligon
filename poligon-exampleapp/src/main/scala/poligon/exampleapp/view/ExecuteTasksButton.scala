package poligon.exampleapp.view

import poligon.exampleapp.EAComp.{Comp, _}
import poligon.exampleapp.MyAct
import poligon.exampleapp.MyAct._
import poligon.exampleapp.services.Services
import poligon.exampleapp.view.ExecuteTasksButton.ExecuteTasksStatus.{InProgress, NotStarted}
import poligon.polyproperty.PropertyObserver.GPropertyObservers
import poligon.polyproperty.{HasSimplePropertyCodec, PropertyWithParent}

object ExecuteTasksButton {

  sealed trait ExecuteTasksStatus

  object ExecuteTasksStatus extends HasSimplePropertyCodec[ExecuteTasksStatus] {

    case object NotStarted extends ExecuteTasksStatus

    case object InProgress extends ExecuteTasksStatus

    case object Success extends ExecuteTasksStatus

    case object Failed extends ExecuteTasksStatus

  }

  class ExecuteTasksContext {
    val executeTaskStatus: PropertyWithParent[ExecuteTasksStatus] = PropertyWithParent(NotStarted)

    def executeTasks: Sin[Unit] = _ =>
      for {
        _ <- executeTaskStatus.set(InProgress).c[GPropertyObservers[Services]]
        isSuccess <- MyAct.deps(_.executeTasksService.executeTasks())
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
