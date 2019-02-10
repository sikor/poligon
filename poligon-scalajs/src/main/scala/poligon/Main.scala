package poligon

import org.scalajs.dom
import org.scalajs.dom.raw.{Element, HTMLElement}

object Main {
  def main(args: Array[String]): Unit = {
    val element: Element = dom.document.createElement("div")
    element.textContent = "Hello world"
    val parent: HTMLElement = dom.document.body
    parent.appendChild(element)
    println("Hello world!")
  }
}
