package exampleapp.view

import com.vaadin.ui.Button
import exampleapp.properties.Comp
import exampleapp.view.ExecuteTasksPresenter.ExecuteTasksStatus.NotStarted

object ExecuteTasksButton {
  def create(presenter: ExecuteTasksPresenter): Comp[Unit] = Comp.dynamicUnit { observed =>
    val button = new Button
    presenter.getModel.listen({ s =>
      button.setCaption(s.toString)
      button.setEnabled(s == NotStarted)
    }, true)(observed)

    button.addClickListener(_ => presenter.executeTasks(observed))
    button
  }
}
