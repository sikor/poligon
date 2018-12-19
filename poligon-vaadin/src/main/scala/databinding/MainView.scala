package databinding

import com.vaadin.ui.{Component, MenuBar, VerticalLayout}
import databinding.properties.Comp
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

  def create(presenter: MainViewPresenter, viewFactory: ViewFactory): Comp = Comp.dynamic { observed: PropertyObservers =>
    val layout = new VerticalLayout()
    presenter.getModel.listen({ menu =>
      val menuBar = new MenuBar()
      menu.menu.categories.foreach { c =>
        val category = menuBar.addItem(c.name, null)
        c.items.foreach { i =>
          category.addItem(i.name, _ => presenter.menuItemSelected(c.name, i.name)(observed))
        }
      }
      MainView.replaceOrAdd(layout, 0, menuBar)
    }, init = true)(observed)

    layout.addComponent(viewFactory.createView(presenter.executeTasksPresenter, observed))

    presenter.getSubpresenter.listen({ subPresenter =>
      MainView.replaceOrAdd(layout, 2, viewFactory.createView(subPresenter, observed))
    }, init = true)(observed)

    layout
  }
}