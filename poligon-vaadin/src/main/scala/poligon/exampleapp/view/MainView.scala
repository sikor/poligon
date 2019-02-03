package poligon
package exampleapp.view

import poligon.exampleapp.HttpServer.Services
import poligon.exampleapp.properties.Binder.LayoutBuilder.Vertical
import poligon.exampleapp.properties.Binder.{Custom, LayoutSettings}
import poligon.exampleapp.properties.{Binder, Comp}
import poligon.exampleapp.view.ExecuteTasksButton.ExecuteTasksContext
import poligon.polyproperty.PropertyWithParent

object MainView {

  class MainViewContext(val services: Services) {
    val executeTasksContext: ExecuteTasksContext = new ExecuteTasksContext(services.executeTasksService)(services.scheduler)
    val menuItems: Seq[(List[String], Comp)] = Seq(List("Menu", "Object Panel") -> ObjectPanelView.create(services))
    val currentPage: PropertyWithParent[Comp] = PropertyWithParent(ObjectPanelView.create(services))
  }

  def create(services: Services): Comp = Comp.factory(create(new MainViewContext(services)))

  private def create(ctx: MainViewContext): Comp = Binder.layout(
    Binder.menuBar(ctx.menuItems, ctx.currentPage.setEnforcingListeners),
    ExecuteTasksButton.create(ctx.executeTasksContext),
    Binder.replaceable(ctx.currentPage.obs, Custom)
  )(Vertical(layoutSettings = LayoutSettings(spacing = true)))

}
