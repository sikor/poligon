package databinding

import com.vaadin.ui.{Component, MenuBar, VerticalLayout}
import databinding.properties.Binder.Custom
import databinding.properties.{Binder, Comp}
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

  def create(presenter: MainViewPresenter, viewFactory: ViewFactory): Comp = Comp.dynamic { po: PropertyObservers =>
    val layout = new VerticalLayout()
    val menuBar = new MenuBar()
    presenter.getModel.listen({ model =>
      menuBar.removeItems()
      model.menu.categories.foreach { c =>
        val category = menuBar.addItem(c.name, null)
        c.items.foreach { i =>
          category.addItem(i.name, _ => presenter.menuItemSelected(c.name, i.name)(po))
        }
      }
    }, init = true)(po)
    val content = Binder.replaceable(presenter.getSubpresenter.map(viewFactory.createView), Custom).bind(po)

    layout.addComponent(menuBar)
    layout.addComponent(viewFactory.createView(presenter.executeTasksPresenter).bind(po))
    layout.addComponent(content)
    layout
  }
}