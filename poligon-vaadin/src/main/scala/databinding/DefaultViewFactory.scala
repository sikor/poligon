package databinding

import com.vaadin.ui.{Component, Label}
import poligon.polyproperty.PropertyObserver.PropertyObservers

object DefaultViewFactory extends ViewFactory {
  override def createView(presenter: Presenter, observed: PropertyObservers): Component = presenter match {
    case m: MainViewPresenter => MainView.create(m, this).bind(observed)
    case o: ObjectsPanelPresenter => ObjectPanelView.createObjectPanelView(o).bind(observed)
    case e: ExecuteTasksPresenter => ExecuteTasksButton.create(e).bind(observed)
    case _ => new Label("view not found")
  }
}
