package databinding.properties

import com.vaadin.ui.{AbstractOrderedLayout, Component}
import io.udash.properties.seq.ReadableSeqProperty
import io.udash.properties.single.ReadableProperty

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
}
