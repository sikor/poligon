package poligon.exampleapp.view

import poligon.exampleapp.properties.Binder.LayoutBuilder.Vertical
import poligon.exampleapp.properties.Binder.{Custom, LayoutSettings}
import poligon.exampleapp.properties.{Binder, Comp}

object MainView {

  def create(presenter: MainViewPresenter): Comp = Binder.layout(
    Binder.menuBar(presenter.menuItems2, presenter.currentPage.setEnforcingListeners),
    ExecuteTasksButton.create(presenter.executeTasksPresenter),
    Binder.replaceable(presenter.currentPage.obs, Custom)
  )(Vertical(layoutSettings = LayoutSettings(spacing = true)))

}
