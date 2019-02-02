package poligon.exampleapp.view

import poligon.exampleapp.properties.Comp
import poligon.exampleapp.services.DmService
import poligon.polyproperty.PropertyWithParent


class MainViewPresenter(val executeTasksPresenter: ExecuteTasksPresenter, dmService: DmService) {
  val menuItems2: Seq[(List[String], Comp)] = Seq(List("Menu", "Object Panel") -> ObjectPanelView.create(dmService))
  val currentPage: PropertyWithParent[Comp] = PropertyWithParent(ObjectPanelView.create(dmService))
}