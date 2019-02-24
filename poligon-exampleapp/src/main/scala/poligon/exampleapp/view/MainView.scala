package poligon
package exampleapp.view

import poligon.comp.Comp
import poligon.exampleapp.view.ExecuteTasksButton.ExecuteTasksContext
import poligon.polyproperty.{Obs, PropertyWithParent}
import Comp._
import poligon.comp.CompFamily.{LayoutSettings, Vertical}
import poligon.comp.CompFamily.MenuTree.MenuValue
import poligon.exampleapp.services.Services

object MainView {

  private val Hello = "hello".tr

  private class MainViewContext(val services: Services) {
    val executeTasksContext: ExecuteTasksContext = new ExecuteTasksContext(services.executeTasksService)
    val menuItems: Seq[(List[String], MenuValue[Comp])] = Seq(List("Menu", "Object Panel") -> MenuValue(ObjectPanelView.create(services)))
    val currentPage: PropertyWithParent[Comp] = PropertyWithParent(ObjectPanelView.create(services))
  }

  def create(services: Services): Comp = Comp.factory(create(new MainViewContext(services)))

  private def create(ctx: MainViewContext): Comp = {
    val helloText: Obs[String] = ctx.services.translator.translate(Hello).toObs.map {
      case Success(s) => s
      case Failure(ex) => ex.getMessage
    }

    layout(
      dynLabel(helloText),
      menuBar(ctx.menuItems, ctx.currentPage.setEnforcingListeners),
      ExecuteTasksButton.create(ctx.executeTasksContext),
      replaceable(ctx.currentPage.obs)
    )(LayoutSettings(Vertical))
  }

}
