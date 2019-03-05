package poligon
package exampleapp.view

import poligon.comp.CompFamily.MenuTree.MenuValue
import poligon.comp.CompFamily.{LayoutSettings, Vertical}
import poligon.exampleapp.EAComp._
import poligon.exampleapp.view.ExecuteTasksButton.ExecuteTasksContext
import poligon.polyproperty.PropertyWithParent

object MainView {

  private val Hello = "hello".tr

  private class MainViewContext {
    val executeTasksContext: ExecuteTasksContext = new ExecuteTasksContext()
    val menuItems: Seq[(List[String], MenuValue[Comp])] = Seq(List("Menu", "Object Panel") -> MenuValue(ObjectPanelView.create))
    val currentPage: PropertyWithParent[Comp] = PropertyWithParent(ObjectPanelView.create)
  }

  def create: Comp = factory(create(new MainViewContext()))

  private def create(ctx: MainViewContext): Comp = {
    layout(
      tLabel(Hello),
      menuBar(ctx.menuItems, ctx.currentPage.setEnforcingListeners),
      ExecuteTasksButton.create(ctx.executeTasksContext),
      replaceable(ctx.currentPage.obs)
    )(LayoutSettings(Vertical))
  }

}
