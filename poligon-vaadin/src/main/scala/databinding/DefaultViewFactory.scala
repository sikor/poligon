package databinding

import com.vaadin.ui.Label
import databinding.properties.Comp

object DefaultViewFactory extends ViewFactory {
  override def createView(presenter: Presenter): Comp = presenter match {
    case m: MainViewPresenter => MainView.create(m, this)
    case o: ObjectsPanelPresenter => ObjectPanelView.createObjectPanelView(o)
    case e: ExecuteTasksPresenter => ExecuteTasksButton.create(e)
    case _ => Comp.static(new Label("view not found"))
  }
}
