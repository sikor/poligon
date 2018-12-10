package databinding

import com.vaadin.ui.{Component, MenuBar, VerticalLayout}
import poligon.polyproperty.PropertyObserver.PropertyObservers

object MainView {
  def replaceOrAdd(layout: VerticalLayout, index: Int, component: Component): Unit = {
    if (layout.getComponentCount > index) {
      val currentMenu = layout.getComponent(index)
      layout.replaceComponent(currentMenu, component)
    } else {
      layout.addComponent(component)
    }
  }
}

class MainView(presenter: MainViewPresenter, viewFactory: ViewFactory, observed: PropertyObservers) extends VerticalLayout {

  presenter.model.listen({ menu =>
    val menuBar = new MenuBar()
    menu.menu.categories.foreach { c =>
      val category = menuBar.addItem(c.name, null)
      c.items.foreach { i =>
        category.addItem(i.name, _ => presenter.menuItemSelected(c.name, i.name))
      }
    }
    MainView.replaceOrAdd(this, 0, menuBar)
  }, initUpdate = true)

  addComponent(viewFactory.createView(presenter.executeTasksPresenter, observed))

  presenter.subPresenter.listen({ subPresenter =>
    MainView.replaceOrAdd(this, 2, viewFactory.createView(subPresenter, observed))
  }, initUpdate = true)
}
