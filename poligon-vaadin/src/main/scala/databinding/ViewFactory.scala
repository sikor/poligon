package databinding

import com.vaadin.ui.Component

trait ViewFactory {
  def createView(presenter: Presenter): Component
}
