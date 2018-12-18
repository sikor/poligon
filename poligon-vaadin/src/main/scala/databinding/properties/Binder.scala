package databinding.properties

import com.vaadin.ui.{AbstractOrderedLayout, Component}
import io.udash.properties.seq.ReadableSeqProperty
import io.udash.properties.single.ReadableProperty
import poligon.polyproperty.{Property, PropertyCodec, SubProperty}


/**
  * TODO:
  * - we need destinction between replacable bindable and constant bindable. Lifetime of the constant bindable is the same
  * as the parent component, whereas replacable bindable can be replaced or removed from parent component.
  * Parent component can be: Layout/Panel. Each element of layout can be replacable or constant. In case of replacable we need
  * placeholder that holds the slot. The problem is that we can't forbid usage of removeComponent or addComponent methods on layout.
  *
  * Problem: Someone can add listener and do not remove it when it is no longer needed.
  * How to overcome that?
  *
  * 1. There might be listeners that are not related to any DOM element.
  * 2. Listeners for components should be automatically removed when their components are detached from DOM:
  * - Layout binder - bind layout to seq and handle cleaning listeners after removal
  *                 - cleaning is needed only if childFactory takes PropertyObservers argument.
  * - Slot binder - bind some slot (panel content, one layout or table cell) - clean listeners after component is replaced
  * - component factory that does not take PropertyObservers does not listen to anything - checked in compile time
  *
  * Solution:
  * - methods that create components with listeners (dynamic components) takes PropertyObservers in argument, otherwise not
  * - To reuse bind* methods we need abstraction over Static and Dynamic components.
  * - sometimes we don't have to create new PropertyObservers for sub dynamic component - only if we know that lifetime of the
  * sub dynamic component is shorter than parent lifetime.
  * - in summary, the only interesting case is when we have dynamic child that we want to replace/remove (seq/union/subpresenter)
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
                                                 childFactory: Property[E] => Comp): Comp = Comp.dynamic { po =>
    require(layout.getComponentCount == 0)
    SubProperty.getSeq(property).foreach { p =>
      layout.addComponent(childFactory(p).bind(po.createSubObservers()))
    }
    property.listenStructure[E] { patch =>
      patch.removed.foreach { _ =>
        val removedComponent = layout.getComponent(patch.idx)
        Comp.unbind(removedComponent)
        layout.removeComponent(removedComponent)
      }
      patch.added.reverse.foreach { a =>
        val c = childFactory(a).bind(po.createSubObservers())
        layout.addComponent(c, patch.idx)
      }
    }(po)
    layout
  }

  def bindSimple[T: PropertyCodec, P <: com.vaadin.data.Property[T] with Component](property: Property[T], label: P): Comp = Comp.dynamic { o =>
    property.listen(v => label.setValue(v), init = true)(o)
    label
  }

}
