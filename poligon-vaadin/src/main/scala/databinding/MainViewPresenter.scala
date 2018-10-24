package databinding

import databinding.MainViewPresenter.{MainViewModel, Menu, MenuCategory, MenuItem}
import io.udash.properties.HasModelPropertyCreator
import io.udash.properties.model.ModelProperty
import io.udash.properties.single.Property

object MainViewPresenter {

  case class MenuItem(name: String)

  object MenuItem extends HasModelPropertyCreator[MenuItem]

  case class MenuCategory(name: String, items: Seq[MenuItem])

  object MenuCategory extends HasModelPropertyCreator[MenuCategory]

  case class Menu(categories: Seq[MenuCategory])

  object Menu extends HasModelPropertyCreator[Menu]

  case class MainViewModel(menu: Menu)

  object MainViewModel extends HasModelPropertyCreator[MainViewModel]

}

class MainViewPresenter extends Presenter {
  val model = ModelProperty(MainViewModel(Menu(Seq(MenuCategory("category 1", Seq(MenuItem("object panel")))))))
  val subPresenter: Property[Presenter] = Property(new ObjectsPanelPresenter)

  private val creators: Map[String, MainViewPresenter => Presenter] =
    Map("category 1.object panel" -> ((parent: MainViewPresenter) => new ObjectsPanelPresenter))

  def menuItemSelected(category: String, name: String): Unit = {
    val presenter = creators(s"$category.$name")(this)
    subPresenter.set(presenter, force = true)
  }

}