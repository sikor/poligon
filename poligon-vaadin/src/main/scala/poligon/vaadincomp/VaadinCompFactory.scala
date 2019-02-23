package poligon.vaadincomp

import com.vaadin.data.Property
import com.vaadin.ui.{Form => _, _}
import poligon.comp.Comp.LayoutModification.{Added, Removed}
import poligon.comp.Comp.MenuTree.{MenuItem, MenuValue}
import poligon.comp.Comp._
import poligon.comp.CompFactory
import poligon.polyproperty.PropertyObserver.RootPropertyObservers
import poligon.polyproperty.{Obs, Sin}

import scala.collection.mutable


object VaadinCompFactory extends CompFactory {
  type ComponentT = Component

  def layout(property: Obs[Seq[LayoutModification[BindableComp]]],
             layoutDescription: LayoutSettings): BindableComp = dynamic { implicit po =>
    val layout = layoutDescription.layoutType match {
      case Vertical => new VerticalLayout()
      case Horizontal => new HorizontalLayout()
      case Form => new FormLayout()
    }
    layout.setSpacing(layoutDescription.spacing)

    property.listen { modifications =>
      modifications.foreach {
        case Removed(index) =>
          val removedComponent = layout.getComponent(index)
          po.deregisterSubObservers(removedComponent)
          layout.removeComponent(removedComponent)
        case Added(index, added) =>
          val c = added.looseBind(po)
          layout.addComponent(c, index)
      }
    }

    layout
  }

  def label(property: Obs[String], styleName: String): BindableComp = bindSimple(property, {
    val l = new Label()
    l.addStyleName(styleName)
    l
  })

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): BindableComp = dynamic { implicit po =>
    val field = new TextField()
    field.setValue(initValue)
    field.addValueChangeListener(_ => onValueSet.push(field.getValue))
    field.setCaption(caption)
    field
  }

  def button(onClick: Sin[Unit], caption: Obs[String], enabled: Obs[Boolean]): BindableComp = dynamic { implicit po =>
    val button = new Button()
    caption.listen { s =>
      button.setCaption(s)
    }
    enabled.listen { e =>
      button.setEnabled(e)
    }
    button.addClickListener(_ => onClick.push(()))
    button
  }

  def checkBox(caption: String, initValue: Boolean, value: Sin[Boolean]): BindableComp = dynamic { implicit po =>
    val cb = new CheckBox(caption, initValue)
    cb.addValueChangeListener((_: Property.ValueChangeEvent) => value.push(cb.getValue))
    cb
  }

  private case class MenuCommand[T](value: MenuItem[T], sin: Sin[T])(implicit po: RootPropertyObservers) extends MenuBar.Command {
    def menuSelected(selectedItem: MenuBar#MenuItem): Unit = {
      sin.push(value.asInstanceOf[MenuValue[T]].value)(po)
    }
  }

  def menuBar[T](menuItems: Seq[(List[String], MenuItem[T])], itemSelected: Sin[T]): BindableComp = dynamic { implicit po =>
    val menuBar = new MenuBar()
    val menuItemsCache = new mutable.HashMap[Vector[String], MenuBar#MenuItem]()
    menuItems.foreach { case (key, value) =>
      var notContainedPrefix = Vector[String]()
      val notContainedSuffix = key.dropWhile { i =>
        notContainedPrefix = notContainedPrefix :+ i
        menuItemsCache.contains(notContainedPrefix)
      }

      var currentPrefix = notContainedPrefix.dropRight(1)
      var currentItem = menuItemsCache.get(currentPrefix)
      notContainedSuffix.foreach { nc =>
        val newItem = currentItem match {
          case Some(parent) => parent.addItem(nc, null)
          case None => menuBar.addItem(nc, null)
        }
        val newPrefix = currentPrefix :+ nc
        menuItemsCache.put(newPrefix, newItem)
        currentPrefix = newPrefix
        currentItem = Some(newItem)
      }

      currentItem.get.setCommand(MenuCommand(value, itemSelected))
    }
    menuBar
  }

  private class SimpleCustomComponent extends CustomComponent {
    def setContent(component: Component): Unit = setCompositionRoot(component)

    def getContent: Component = getCompositionRoot
  }

  def replaceable(property: Obs[BindableComp]): BindableComp = dynamic { implicit po =>
    val wrapper = new SimpleCustomComponent()
    property.listen { comp =>
      val component = comp.looseBind(po)
      po.deregisterSubObservers(wrapper.getContent)
      wrapper.setContent(component)
    }
    wrapper
  }

  private def bindSimple[T, P <: com.vaadin.data.Property[T] with Component]
  (property: Obs[T], label: => P): BindableComp = dynamic { implicit o =>
    val l = label
    property.listen(v => l.setValue(v))
    l
  }
}