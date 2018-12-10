package databinding

import com.vaadin.ui.{Component, Label}
import poligon.polyproperty.PropertyObserver.PropertyObservers

object DefaultViewFactory extends ViewFactory {
  override def createView(presenter: Presenter, observed: PropertyObservers): Component = presenter match {
    case m: MainViewPresenter => new MainView(m, this, observed)
    case o: ObjectsPanelPresenter => ObjectPanelView.createObjectPanelView(o)
    case e: ExecuteTasksPresenter => new ExecuteTasksView(e)(observed)
    case _ => new Label("view not found")
  }
}
