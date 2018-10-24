package databinding

import com.vaadin.ui.Button
import databinding.ExecuteTasksPresenter.ExecuteTasksStatus.NotStarted

class ExecuteTasksView(presenter: ExecuteTasksPresenter) extends Button {
  presenter.model.listen({ s =>
    setCaption(s.toString)
    setEnabled(s == NotStarted)
  }, initUpdate = true)

  addClickListener(_ => presenter.executeTasks())
}
