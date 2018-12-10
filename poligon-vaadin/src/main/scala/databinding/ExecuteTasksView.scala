package databinding

import com.vaadin.ui.Button
import databinding.ExecuteTasksPresenter.ExecuteTasksStatus.NotStarted
import poligon.polyproperty.PropertyObserver.PropertyObservers

class ExecuteTasksView(presenter: ExecuteTasksPresenter)(implicit observed: PropertyObservers) extends Button {
  presenter.getModel.listen({ s =>
    setCaption(s.toString)
    setEnabled(s == NotStarted)
  }, true)

  addClickListener(_ => presenter.executeTasks)
}
