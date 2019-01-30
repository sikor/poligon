package poligon.exampleapp.view

import com.vaadin.ui.Button
import poligon.exampleapp.properties.Comp
import poligon.exampleapp.view.ExecuteTasksPresenter.ExecuteTasksStatus.NotStarted

object ExecuteTasksButton {
  def create(presenter: ExecuteTasksPresenter): Comp = Comp.dynamic { observed =>
    val button = new Button
    presenter.model.listen({ s =>
      button.setCaption(s.toString)
      button.setEnabled(s == NotStarted)
    }, true)(observed)

    button.addClickListener(_ => presenter.executeTasks(observed))
    button
  }
}
