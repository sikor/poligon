package databinding.properties

import com.vaadin.ui.{AbstractOrderedLayout, Component, VerticalLayout}
import io.udash.properties.seq.ReadableSeqProperty
import io.udash.properties.single.ReadableProperty
import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty.{Property, PropertyCodec, SubProperty}


/**
  * TODO:
  * - we need destinction between replacable bindable and constant bindable. Lifetime of the constant bindable is the same
  * as the parent component, whereas replacable bindable can be replaced or removed from parent component.
  * Parent component can be: Layout/Panel. Each element of layout can be replacable or constant. In case of replacable we need
  * placeholder that holds the slot. The problem is that we can't forbid usage of removeComponent or addComponent methods on layout.
  * The risk is that someone will bind bindable and never clean it.
  */
object Binder {
  def bindLayoutStructure[L <: AbstractOrderedLayout, T, P <: ReadableProperty[T]](
                                                                                    property: ReadableSeqProperty[T, P],
                                                                                    layout: L)(
                                                                                    childFactory: P => Component): L = {
    val startIndex = layout.getComponentCount
    property.elemProperties.foreach { p =>
      layout.addComponent(childFactory(p))
    }
    property.listenStructure { patch =>
      patch.removed.foreach { _ =>
        layout.removeComponent(layout.getComponent(startIndex + patch.idx))
      }
      patch.added.reverse.foreach { a =>
        val c = childFactory(a)
        layout.addComponent(c, startIndex + patch.idx)
      }
    }
    layout
  }

  def bindVaadinProperty[T, P <: com.vaadin.data.Property[T]](property: ReadableProperty[T], label: P): P = {
    property.listen(v => label.setValue(v), initUpdate = true)
    label
  }

  def bindLayout[L <: AbstractOrderedLayout, E](
                                                 property: Property[Seq[E]],
                                                 layout: L)(
                                                 childFactory: Property[E] => Component)(implicit o: PropertyObservers): L = {
    val startIndex = layout.getComponentCount
    SubProperty.getSeq(property).foreach { p =>
      layout.addComponent(childFactory(p))
    }
    property.listenStructure[E] { patch =>
      patch.removed.foreach { _ =>
        layout.removeComponent(layout.getComponent(startIndex + patch.idx))
      }
      patch.added.reverse.foreach { a =>
        val c = childFactory(a)
        layout.addComponent(c, startIndex + patch.idx)
      }
    }
    layout
  }

  def bindSimple[T: PropertyCodec, P <: com.vaadin.data.Property[T]](property: Property[T], label: P)(implicit o: PropertyObservers): P = {
    property.listen(v => label.setValue(v), init = true)
    label
  }

  trait Bindable[C] {
    def bind(p: PropertyObservers): C
  }

  object Bindable {
    def create[C](creator: PropertyObservers => C): Bindable[C] = (p: PropertyObservers) => creator(p)
  }

  def layout[T](property: Property[Seq[T]], childFactory: Property[T] => Bindable[Component]): Bindable[AbstractOrderedLayout] = Bindable.create { po =>
    val l = new VerticalLayout
    property.getSeq[T].foreach { p =>
      val child = childFactory(p)
      val childComponent = child.bind(po)
      l.addComponent(childComponent)
    }
    l
  }
}
