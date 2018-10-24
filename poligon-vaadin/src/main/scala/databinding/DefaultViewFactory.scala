package databinding

import com.vaadin.ui.{Component, Label}

object DefaultViewFactory extends ViewFactory {
  override def createView(presenter: Presenter): Component = presenter match {
    case m: MainViewPresenter => new MainView(m, this)
    case o: ObjectsPanelPresenter => ObjectPanelView.createObjectPanelView(o)
    case e: ExecuteTasksPresenter => new ExecuteTasksView(e)
    case _ => new Label("view not found")
  }
}
