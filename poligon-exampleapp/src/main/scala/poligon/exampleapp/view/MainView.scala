package poligon.exampleapp.view

import poligon.comp.Comp
import poligon.exampleapp.view.ExecuteTasksButton.ExecuteTasksContext
import poligon.polyproperty.PropertyWithParent
import Comp._
import poligon.comp.Comp.MenuTree.MenuValue
import poligon.exampleapp.services.Services

object MainView {

  class MainViewContext(val services: Services) {
    val executeTasksContext: ExecuteTasksContext = new ExecuteTasksContext(services.executeTasksService)(services.scheduler)
    val menuItems: Seq[(List[String], MenuValue[Comp])] = Seq(List("Menu", "Object Panel") -> MenuValue(ObjectPanelView.create(services)))
    val currentPage: PropertyWithParent[Comp] = PropertyWithParent(ObjectPanelView.create(services))
  }

  def create(services: Services): Comp = Comp.factory(create(new MainViewContext(services)))

  private def create(ctx: MainViewContext): Comp = layout(
    menuBar(ctx.menuItems, ctx.currentPage.setEnforcingListeners),
    ExecuteTasksButton.create(ctx.executeTasksContext),
    replaceable(ctx.currentPage.obs)
  )(LayoutSettings(Vertical))

}
