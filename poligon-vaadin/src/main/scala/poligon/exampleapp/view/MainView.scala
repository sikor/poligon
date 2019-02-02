package poligon.exampleapp.view

import poligon.exampleapp.properties.Binder.LayoutBuilder.Vertical
import poligon.exampleapp.properties.Binder.{Custom, LayoutSettings}
import poligon.exampleapp.properties.{Binder, Comp}
import poligon.exampleapp.services.DmService
import poligon.polyproperty.PropertyWithParent

object MainView {

  class MainViewContext(val executeTasksPresenter: ExecuteTasksPresenter, dmService: DmService) {
    val menuItems: Seq[(List[String], Comp)] = Seq(List("Menu", "Object Panel") -> ObjectPanelView.create(dmService))
    val currentPage: PropertyWithParent[Comp] = PropertyWithParent(ObjectPanelView.create(dmService))
  }

  def create(presenter: MainViewContext): Comp = Binder.layout(
    Binder.menuBar(presenter.menuItems, presenter.currentPage.setEnforcingListeners),
    ExecuteTasksButton.create(presenter.executeTasksPresenter),
    Binder.replaceable(presenter.currentPage.obs, Custom)
  )(Vertical(layoutSettings = LayoutSettings(spacing = true)))

}
