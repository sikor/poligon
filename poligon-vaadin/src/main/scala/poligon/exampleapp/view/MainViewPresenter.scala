package poligon.exampleapp.view

import poligon.exampleapp.services.DmService
import poligon.exampleapp.view.MainView.MainViewContentPresenter
import poligon.exampleapp.view.MainViewPresenter.{MainViewModel, Menu, MenuCategory, MenuItem}
import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty.{HasSimplePropertyCodec, PropertyWithParent}

object MainViewPresenter {

  case class MenuItem(name: String)

  case class MenuCategory(name: String, items: Seq[MenuItem])

  case class Menu(categories: Seq[MenuCategory])

  case class MainViewModel(menu: Menu)

  object MainViewModel extends HasSimplePropertyCodec[MainViewModel]

}

class MainViewPresenter(val executeTasksPresenter: ExecuteTasksPresenter, dmService: DmService) {
  val menuItems = MainViewModel(Menu(Seq(MenuCategory("category 1", Seq(MenuItem("object panel"))))))
  val subPresenter: PropertyWithParent[MainViewContentPresenter] = PropertyWithParent(new ObjectsPanelPresenter(dmService))

  private val creators: Map[String, MainViewPresenter => MainViewContentPresenter] =
    Map("category 1.object panel" -> ((parent: MainViewPresenter) => new ObjectsPanelPresenter(dmService)))

  def menuItemSelected(category: String, name: String)(implicit po: PropertyObservers): Unit = {
    val presenter = creators(s"$category.$name")(this)
    subPresenter.set(presenter)
  }

}