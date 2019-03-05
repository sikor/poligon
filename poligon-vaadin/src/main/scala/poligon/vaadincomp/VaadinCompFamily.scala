package poligon.vaadincomp

import com.vaadin.data.Property
import com.vaadin.ui.{Form => _, _}
import monix.eval.Task
import poligon.comp.BindableComp.BindableComp
import poligon.comp.CompFamily.LayoutModification.{Added, Removed}
import poligon.comp.CompFamily.MenuTree.{MenuItem, MenuValue}
import poligon.comp.CompFamily._
import poligon.comp.{BindableComp, CompFamily}
import poligon.polyproperty.Act
import poligon.polyproperty.Act.GSin
import poligon.polyproperty.Obs.BObs
import poligon.polyproperty.PropertyObserver.GPropertyObservers

import scala.collection.mutable


object VaadinCompFamily extends CompFamily[Component] {

  def layout[D](property: BObs[Seq[LayoutModification[BindableComp[Component, D]]], D],
                layoutDescription: LayoutSettings): BindableComp[Component, D] = dynamic { po =>
    val layout = layoutDescription.layoutType match {
      case Vertical => new VerticalLayout()
      case Horizontal => new HorizontalLayout()
      case Form => new FormLayout()
    }
    layout.setSpacing(layoutDescription.spacing)

    property.listen({ modifications =>
      gatherModifications(modifications, po).map(_.foreach {
        case Removed(index) =>
          val removedComponent = layout.getComponent(index)
          po.deregisterSubObservers(removedComponent)
          layout.removeComponent(removedComponent)
        case Added(index, added) =>
          layout.addComponent(added, index)
      })
    }, po).map(_ => layout)
  }

  def label[D](property: BObs[String, D], styleName: String): BindableComp[Component, D] = bindSimple(property, {
    val l = new Label()
    l.addStyleName(styleName)
    l
  })

  def textField[D](caption: String, initValue: String, onValueSet: GSin[String, D]): BindableComp[Component, D] = simple { implicit po =>
    val field = new TextField()
    field.setValue(initValue)
    field.addValueChangeListener(_ => Act.push(field.getValue, onValueSet))
    field.setCaption(caption)
    field
  }

  def button[D](onClick: GSin[Unit, D], caption: BObs[String, D], enabled: BObs[Boolean, D]): BindableComp[Component, D] = dynamic { implicit po =>
    val button = new Button()
    button.addClickListener(_ => Act.push((), onClick))
    val t1 = caption.listenNow(po) { s =>
      button.setCaption(s)
    }
    val t2 = enabled.listenNow(po) { e =>
      button.setEnabled(e)
    }
    Task.gatherUnordered(List(t1, t2)).map(_ => button)
  }

  def checkBox[D](caption: String, initValue: Boolean, l: GSin[Boolean, D]): BindableComp[Component, D] = simple { implicit po =>
    val cb = new CheckBox(caption, initValue)
    cb.addValueChangeListener((_: Property.ValueChangeEvent) => Act.push[Boolean, D](cb.getValue, l))
    cb
  }

  private case class MenuCommand[T, D](value: MenuItem[T], sin: GSin[T, D])(implicit po: GPropertyObservers[D]) extends MenuBar.Command {
    def menuSelected(selectedItem: MenuBar#MenuItem): Unit = {
      Act.push(value.asInstanceOf[MenuValue[T]].value, sin)
    }
  }

  def menuBar[T, D](menuItems: Seq[(List[String], MenuItem[T])], itemSelected: GSin[T, D]): BindableComp[Component, D] = simple { implicit po =>
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

  def replaceable[D](property: BObs[BindableComp[Component, D], D]): BindableComp[Component, D] = dynamic { implicit po =>
    val wrapper = new SimpleCustomComponent()
    property.listenOn(po) { comp =>
      BindableComp.bind(comp, po).map { component =>
        po.deregisterSubObservers(wrapper.getContent)
        wrapper.setContent(component)
      }
    }.map(_ => wrapper)
  }

  private def bindSimple[T, P <: com.vaadin.data.Property[T] with Component, D]
  (property: BObs[T, D], label: => P): BindableComp[Component, D] = dynamic { implicit po =>
    val l = label
    property.listenNow(po)(v => l.setValue(v)).map(_ => l)
  }
}