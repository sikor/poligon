package databinding.properties

import com.vaadin.ui._
import databinding.properties.Binder.LayoutDescription.{Horizontal, Vertical}
import poligon.polyproperty.{Obs, Property, PropertyCodec, SubProperty}


/**
  * TODO:
  * Handling forms:
  * Option 1: Forms orchestration in View, parent component orchestrates all fields and submit button. On submit it collects data and call
  * appropriate method on presenter.
  * - Need a way for component to communicate with its children and potentially siblings
  *
  * Option 2: Forms orchestration in Presenter, presenter creates model for form data that can be modified from view, this model
  * should map one to one to fields in view. On submit presenter collects data from model and calls some action.
  * - Need a way for component to modify properties.
  * - Have to add validation functionality to properties
  */
object Binder {

  sealed trait LayoutDescription

  object LayoutDescription {

    case object Vertical extends LayoutDescription

    case object Horizontal extends LayoutDescription

    case object Form extends LayoutDescription

  }

  def layout[E](
                 property: Property[Seq[E]],
                 layoutDescription: LayoutDescription = Vertical)(
                 childFactory: Property[E] => Comp): Comp = Comp.dynamic { po =>
    val layout = layoutDescription match {
      case Vertical => new VerticalLayout()
      case Horizontal => new HorizontalLayout()
      case LayoutDescription.Form => new FormLayout()
    }

    require(layout.getComponentCount == 0)

    SubProperty.getSeq(property).foreach { p =>
      layout.addComponent(childFactory(p).looseBind(po))
    }
    property.listenStructure[E] { patch =>
      patch.removed.foreach { _ =>
        val removedComponent = layout.getComponent(patch.idx)
        po.deregisterSubObservers(removedComponent)
        layout.removeComponent(removedComponent)
      }
      patch.added.reverse.foreach { a =>
        val c = childFactory(a).looseBind(po)
        layout.addComponent(c, patch.idx)
      }
    }(po)
    layout
  }

  def label(property: Obs[String], styleName: String = ""): Comp = bindSimple(property, {
    val l = new Label()
    l.addStyleName(styleName)
    l
  })

  private def bindSimple[T: PropertyCodec, P <: com.vaadin.data.Property[T] with Component](property: Obs[T], label: => P): Comp =
    Comp.dynamic { o =>
      val l = label
      property.listen(v => l.setValue(v), init = true)(o)
      l
    }

  sealed trait WrapperDescription

  case object Panel extends WrapperDescription

  case object Custom extends WrapperDescription

  private class SimpleCustomComponent extends CustomComponent {
    def setContent(component: Component): Unit = setCompositionRoot(component)

    def getContent: Component = getCompositionRoot
  }

  def replaceable(property: Obs[Comp], wrapperDescription: WrapperDescription): Comp = Comp.dynamic { po =>
    val wrapper = wrapperDescription match {
      case Panel => new Panel()
      case Custom => new SimpleCustomComponent()
    }
    property.listen({ c =>
      val component = c.looseBind(po)
      wrapper match {
        case p: Panel =>
          po.deregisterSubObservers(p.getContent)
          p.setContent(component)
        case s: SimpleCustomComponent =>
          po.deregisterSubObservers(s.getContent)
          s.setContent(component)
      }
    }, init = true)(po)
    wrapper
  }
}
