package databinding.properties

import com.avsystem.commons.misc.OptArg
import com.vaadin.ui._
import databinding.properties.Binder.LayoutDescription.{Horizontal, Vertical}
import databinding.properties.Comp.Bound
import poligon.polyproperty.{Obs, Property, PropertyCodec, SubProperty}

import scala.collection.mutable.ArrayBuffer


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

  case class BaseSettings(caption: OptArg[String] = OptArg.Empty) {
    def setOn[T <: Component](c: T): T = {
      caption.foreach(c.setCaption)
      c
    }
  }

  object LayoutDescription {

    case object Vertical extends LayoutDescription

    case object Horizontal extends LayoutDescription

    case class Form(settings: BaseSettings = BaseSettings()) extends LayoutDescription

  }

  def layout[E, V](
                    property: Property[Seq[E]],
                    layoutDescription: LayoutDescription = Vertical)(
                    childFactory: Property[E] => Comp[V]): Comp[Seq[V]] = Comp.dynamic { po =>
    val layout = layoutDescription match {
      case Vertical => new VerticalLayout()
      case Horizontal => new HorizontalLayout()
      case LayoutDescription.Form(settings) => settings.setOn(new FormLayout())
    }

    val bounds = new ArrayBuffer[Bound[V]]()

    SubProperty.getSeq(property).foreach { p =>
      val bound = childFactory(p).looseBind(po)
      layout.addComponent(bound.comp)
      bounds += bound
    }
    property.listenStructure[E] { patch =>
      patch.removed.foreach { _ =>
        val removedComponent = layout.getComponent(patch.idx)
        po.deregisterSubObservers(removedComponent)
        layout.removeComponent(removedComponent)
      }
      bounds.remove(patch.idx, patch.removed.size)

      patch.added.reverse.foreach { a =>
        val c = childFactory(a).looseBind(po)
        layout.addComponent(c.comp, patch.idx)
        bounds.insert(patch.idx, c)
      }
    }(po)
    Comp.bound(bounds.iterator.map(_.get).toVector, layout)
  }

  def label(property: Obs[String], styleName: String = ""): Comp[Unit] = bindSimple(property, {
    val l = new Label()
    l.addStyleName(styleName)
    l
  })

  private def bindSimple[T: PropertyCodec, P <: com.vaadin.data.Property[T] with Component](property: Obs[T], label: => P): Comp[Unit] =
    Comp.dynamicUnit { o =>
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

  def replaceable[V](property: Obs[Comp[V]], wrapperDescription: WrapperDescription): Comp[V] = Comp.dynamic[V] { po =>
    val wrapper = wrapperDescription match {
      case Panel => new Panel()
      case Custom => new SimpleCustomComponent()
    }
    var bound: Bound[V] = null
    property.listen({ c =>
      val component = c.looseBind(po)
      bound = component
      wrapper match {
        case p: Panel =>
          po.deregisterSubObservers(p.getContent)
          p.setContent(component.comp)
        case s: SimpleCustomComponent =>
          po.deregisterSubObservers(s.getContent)
          s.setContent(component.comp)
      }
    }, init = true)(po)
    Comp.bound(bound.get, wrapper)
  }
}
