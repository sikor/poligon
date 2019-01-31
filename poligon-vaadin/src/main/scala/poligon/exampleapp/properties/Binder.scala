package poligon.exampleapp.properties

import com.avsystem.commons.misc.OptArg
import com.vaadin.ui._
import poligon.exampleapp.properties.Binder.LayoutBuilder.Vertical
import poligon.polyproperty.PropertyCodec.PropertyChange.{Added, Removed}
import poligon.polyproperty.PropertyWithParent.Struct
import poligon.polyproperty._


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
  *
  *
  * Reusing components:
  * 1. Method one - component uses Obs and lambdas as their api
  * 2. Method two - component define its model
  * 3. Method three - model can implement interfaces that allows them to be used in components
  */
object Binder {

  type AnyLayoutBuilder = LayoutBuilder[AbstractOrderedLayout]

  case class BaseSettings(caption: OptArg[String] = OptArg.Empty) {
    def setOn[T <: Component](c: T): T = {
      caption.foreach(c.setCaption)
      c
    }
  }

  case class LayoutSettings(spacing: Boolean = false) {
    def setOn(layout: AbstractOrderedLayout): Unit = {
      layout.setSpacing(spacing)
    }
  }

  sealed trait LayoutBuilder[+T <: AbstractOrderedLayout] {
    def layoutSettings: LayoutSettings

    def baseSettings: BaseSettings

    def create(comps: Component*): T

    final def build(comps: Component*): T = {
      val l = create(comps: _*)
      baseSettings.setOn(l)
      layoutSettings.setOn(l)
      l
    }
  }

  object LayoutBuilder {

    case class Vertical(layoutSettings: LayoutSettings = LayoutSettings(), baseSettings: BaseSettings = BaseSettings())
      extends LayoutBuilder[VerticalLayout] {
      def create(comps: Component*): VerticalLayout = new VerticalLayout(comps: _*)
    }

    case class Horizontal(layoutSettings: LayoutSettings = LayoutSettings(), baseSettings: BaseSettings = BaseSettings())
      extends LayoutBuilder[HorizontalLayout] {
      def create(comps: Component*): HorizontalLayout = new HorizontalLayout(comps: _*)
    }

    case class Form(layoutSettings: LayoutSettings = LayoutSettings(), baseSettings: BaseSettings = BaseSettings())
      extends LayoutBuilder[FormLayout] {
      def create(comps: Component*): FormLayout = new FormLayout(comps: _*)
    }

  }

  def layout(comps: Comp*)(builder: AnyLayoutBuilder = Vertical()): Comp = Comp.dynamic { implicit po =>
    builder.create(comps.map(_.bind(po)): _*)
  }

  def dynLayout[V](
                 property: Obs[Struct[V]],
                 layoutDescription: AnyLayoutBuilder = Vertical())(
                 childFactory: PropertyWithParent[V] => Comp): Comp =
    Comp.dynamic { implicit po =>
      val layout = layoutDescription.build()

      property.listen { patch =>
        patch.modifications.foreach {
          case Removed(removed) =>
            val removedComponent = layout.getComponent(removed.index)
            po.deregisterSubObservers(removedComponent)
            layout.removeComponent(removedComponent)
          case Added(added) =>
            val c = childFactory(added.value).looseBind(po)
            layout.addComponent(c, added.index)
        }
      }

      layout
    }

  def label(value: String, styleName: String = ""): Comp = Comp.static(new Label(value))

  def dynLabel(property: Obs[String], styleName: String = ""): Comp = bindSimple(property, {
    val l = new Label()
    l.addStyleName(styleName)
    l
  })

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): Comp = Comp.dynamic { implicit po =>
    val field = new TextField()
    field.setValue(initValue)
    field.addValueChangeListener(_ => onValueSet.set(field.getValue))
    field.setCaption(caption)
    field
  }

  def textField(caption: String, property: PropertyWithParent[String]): Comp =
    textField(caption, property.read, property.sin)


  def button(caption: String, onClick: Sin[Unit]): Comp = Comp.dynamic { implicit po =>
    val button = new Button(caption)
    button.addClickListener(_ => onClick.set(()))
    button
  }

  private def bindSimple[T: PropertyCodec, P <: com.vaadin.data.Property[T] with Component](property: Obs[T], label: => P): Comp =
    Comp.dynamic { o =>
      val l = label
      property.listen(v => l.setValue(v))(o)
      l
    }

  sealed trait WrapperDescription

  case object Panel extends WrapperDescription

  case object Custom extends WrapperDescription

  private class SimpleCustomComponent extends CustomComponent {
    def setContent(component: Component): Unit = setCompositionRoot(component)

    def getContent: Component = getCompositionRoot
  }

  def replaceable(property: Obs[Comp], wrapperDescription: WrapperDescription): Comp = Comp.dynamic {
    implicit po =>
      val wrapper = wrapperDescription match {
        case Panel => new Panel()
        case Custom => new SimpleCustomComponent()
      }
      property.listen({
        c =>
          val component = c.looseBind(po)
          wrapper match {
            case p: Panel =>
              po.deregisterSubObservers(p.getContent)
              p.setContent(component)
            case s: SimpleCustomComponent =>
              po.deregisterSubObservers(s.getContent)
              s.setContent(component)
          }
      })
      wrapper
  }
}
