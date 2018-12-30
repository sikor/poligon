package databinding

import databinding.properties.Comp

trait ViewFactory {
  def createView(presenter: Presenter): Comp
}
