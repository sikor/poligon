package poligon.exampleapp.properties

import com.avsystem.commons.misc.OptArg
import com.vaadin.ui._
import poligon.exampleapp.properties.Binder.LayoutDescription.{Horizontal, Vertical}
import poligon.exampleapp.properties.Comp.Bound
import poligon.polyproperty.Property.PropertyChange.{Added, Removed}
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

  def layout[E](
                 property: Property[Seq[E]],
                 layoutDescription: LayoutDescription = Vertical)(
                 childFactory: Property[E] => Comp[Unit]): Comp[Unit] = Comp.dynamic { po =>
    val layout = layoutDescription match {
      case Vertical => new VerticalLayout()
      case Horizontal => new HorizontalLayout()
      case LayoutDescription.Form(settings) => settings.setOn(new FormLayout())
    }


    SubProperty.getSeq(property).foreach { p =>
      val bound = childFactory(p).looseBind(po)
      layout.addComponent(bound.comp)
    }
    property.listenStructure[E] { patch =>
      patch.modifications.foreach {
        case Removed(removed) =>
          val removedComponent = layout.getComponent(removed.index)
          po.deregisterSubObservers(removedComponent)
          layout.removeComponent(removedComponent)
        case Added(added) =>
          val c = childFactory(added.value).looseBind(po)
          layout.addComponent(c.comp, added.index)
      }
    }(po)
    Comp.unitBound(layout)
  }

  def label(property: Obs[String], styleName: String = ""): Comp[Unit] = bindSimple(property, {
    val l = new Label()
    l.addStyleName(styleName)
    l
  })

  def textField(caption: String, property: Obs[String], onValueSet: String => Unit): Comp[Unit] = Comp.dynamic { po =>
    val field = new TextField()
    field.addValueChangeListener(_ => onValueSet(field.getValue))
    field.setCaption(caption)
    property.listen(v => field.setValue(v), init = true)(po)
    Comp.unitBound(field)
  }

  private def bindSimple[T: PropertyCodec, P <: com.vaadin.data.Property[T] with Component](property: Obs[T], label: => P): Comp[Unit] =
    Comp.dynamicUnit {
      o =>
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

  def replaceable[V](property: Obs[Comp[V]], wrapperDescription: WrapperDescription): Comp[V] = Comp.dynamic[V] {
    po =>
      val wrapper = wrapperDescription match {
        case Panel => new Panel()
        case Custom => new SimpleCustomComponent()
      }
      var bound: Bound[V] = null
      property.listen({
        c =>
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
