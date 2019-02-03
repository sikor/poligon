package poligon.exampleapp.view

import com.vaadin.ui.Button
import poligon.exampleapp.properties.{Binder, Comp}
import poligon.exampleapp.view.ExecuteTasksPresenter.ExecuteTasksStatus.NotStarted

object ExecuteTasksButton {

  def create2(presenter: ExecuteTasksPresenter): Comp =
    Binder.button(presenter.executeTaskStatus.map(s => s.toString), ???, presenter.executeTaskStatus.map(s => s == NotStarted))

  def create(presenter: ExecuteTasksPresenter): Comp = Comp.dynamic { observed =>
    val button = new Button
    presenter.executeTaskStatus.listen({ s =>
      button.setCaption(s.toString)
      button.setEnabled(s == NotStarted)
    }, true)(observed)

    button.addClickListener(_ => presenter.executeTasks.push(())(observed))
    button
  }
}
