package databinding

import com.vaadin.ui.{Component, MenuBar, VerticalLayout}

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

class MainView(presenter: MainViewPresenter, viewFactory: ViewFactory) extends VerticalLayout {

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

  presenter.subPresenter.listen({ subPresenter =>
    MainView.replaceOrAdd(this, 1, viewFactory.createView(subPresenter))
  }, initUpdate = true)
}
