package databinding

import com.vaadin.ui.Component
import poligon.polyproperty.PropertyObserver.PropertyObservers

trait ViewFactory {
  def createView(presenter: Presenter, observed: PropertyObservers): Component
}
