package poligon.exampleapp.view

import com.vaadin.ui.{MenuBar, VerticalLayout}
import poligon.exampleapp.properties.Binder.Custom
import poligon.exampleapp.properties.{Binder, Comp}
import poligon.exampleapp.view.MainView.MainViewContentPresenter.ObjectsPanelContent
import poligon.polyproperty.HasSimplePropertyCodec
import poligon.polyproperty.PropertyObserver.PropertyObservers

object MainView {

  sealed trait MainViewContentPresenter

  object MainViewContentPresenter extends HasSimplePropertyCodec[MainViewContentPresenter] {

    trait ObjectsPanelContent extends MainViewContentPresenter {
      impl: ObjectsPanelPresenter =>
      def get: ObjectsPanelPresenter = impl
    }

  }

  private def createContent(presenter: MainViewContentPresenter): Comp[Unit] = presenter match {
    case o: ObjectsPanelContent => ObjectPanelView.createObjectPanelView(o.get)
  }

  def create(presenter: MainViewPresenter): Comp[Unit] = Comp.dynamicUnit { po: PropertyObservers =>
    val menuBar = new MenuBar()
    presenter.model.listen({ model =>
      menuBar.removeItems()
      model.menu.categories.foreach { c =>
        val category = menuBar.addItem(c.name, null)
        c.items.foreach { i =>
          category.addItem(i.name, _ => presenter.menuItemSelected(c.name, i.name)(po))
        }
      }
    }, init = true)(po)
    val content = Binder.replaceable(presenter.subPresenter.map(createContent), Custom).bind(po)

    val layout = new VerticalLayout()
    layout.addComponent(menuBar)
    layout.addComponent(ExecuteTasksButton.create(presenter.executeTasksPresenter).bind(po).comp)
    layout.addComponent(content.comp)
    layout
  }
}
