package databinding.properties

import com.vaadin.ui.{AbstractOrderedLayout, Component, Label}
import io.udash.properties.seq.ReadableSeqProperty
import io.udash.properties.single.{Property, ReadableProperty}

object Binder {
  def bind[T, P <: ReadableProperty[T]](
                                         property: ReadableSeqProperty[T, P],
                                         layout: AbstractOrderedLayout,
                                         childFactory: P => Component): Unit = {
    property.elemProperties.foreach { p =>
      layout.addComponent(childFactory(p))
    }
    property.listenStructure { patch =>
      if (patch.clearsProperty) {
        layout.removeAllComponents()
      } else {
        patch.added.reverse.foreach { a =>
          val c = childFactory(a)
          layout.addComponent(c, patch.idx)
        }
        patch.removed.foreach { _ =>
          layout.removeComponent(layout.getComponent(patch.idx))
        }
      }
    }
  }

  def bindLabel(property: Property[String], label: Label): Unit = {
    property.listen(v => label.setValue(v), initUpdate = true)
  }
}
