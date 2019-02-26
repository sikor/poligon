package poligon.vaadincomp

import com.vaadin.data.Property
import com.vaadin.ui.{Form => _, _}
import monix.eval.Task
import poligon.comp.CompFamily
import poligon.comp.CompFamily.LayoutModification.{Added, Removed}
import poligon.comp.CompFamily.MenuTree.{MenuItem, MenuValue}
import poligon.comp.CompFamily._
import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty.{Obs, Sin}

import scala.collection.mutable


object VaadinCompFamily extends CompFamily[Component] {

  def layout(property: Obs[Seq[LayoutModification[BComp]]],
             layoutDescription: LayoutSettings): BComp = dynamic { implicit po =>
    val layout = layoutDescription.layoutType match {
      case Vertical => new VerticalLayout()
      case Horizontal => new HorizontalLayout()
      case Form => new FormLayout()
    }
    layout.setSpacing(layoutDescription.spacing)

    property.listen { modifications =>
      gatherModifications(modifications).map(_.foreach {
        case Removed(index) =>
          val removedComponent = layout.getComponent(index)
          po.deregisterSubObservers(removedComponent)
          layout.removeComponent(removedComponent)
        case Added(index, added) =>
          layout.addComponent(added, index)
      })
    }.map(_ => layout)
  }

  def label(property: Obs[String], styleName: String): BComp = bindSimple(property, {
    val l = new Label()
    l.addStyleName(styleName)
    l
  })

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): BComp = simple { implicit po =>
    val field = new TextField()
    field.setValue(initValue)
    field.addValueChangeListener(_ => onValueSet.push(field.getValue))
    field.setCaption(caption)
    field
  }

  def button(onClick: Sin[Unit], caption: Obs[String], enabled: Obs[Boolean]): BComp = dynamic { implicit po =>
    val button = new Button()
    button.addClickListener(_ => onClick.push(()))
    val t1 = caption.listenNow { s =>
      button.setCaption(s)
    }
    val t2 = enabled.listenNow { e =>
      button.setEnabled(e)
    }
    Task.gatherUnordered(List(t1, t2)).map(_ => button)
  }

  def checkBox(caption: String, initValue: Boolean, value: Sin[Boolean]): BComp = simple { implicit po =>
    val cb = new CheckBox(caption, initValue)
    cb.addValueChangeListener((_: Property.ValueChangeEvent) => value.push(cb.getValue))
    cb
  }

  private case class MenuCommand[T](value: MenuItem[T], sin: Sin[T])(implicit po: PropertyObservers) extends MenuBar.Command {
    def menuSelected(selectedItem: MenuBar#MenuItem): Unit = {
      sin.push(value.asInstanceOf[MenuValue[T]].value)(po)
    }
  }

  def menuBar[T](menuItems: Seq[(List[String], MenuItem[T])], itemSelected: Sin[T]): BComp = simple { implicit po =>
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

  def replaceable(property: Obs[BComp]): BComp = dynamic { implicit po =>
    val wrapper = new SimpleCustomComponent()
    property.listen { comp =>
      comp.bind(po).map { component =>
        po.deregisterSubObservers(wrapper.getContent)
        wrapper.setContent(component)
      }
    }.map(_ => wrapper)
  }

  private def bindSimple[T, P <: com.vaadin.data.Property[T] with Component]
  (property: Obs[T], label: => P): BComp = dynamic { implicit po =>
    val l = label
    property.listenNow(v => l.setValue(v)).map(_ => l)
  }
}