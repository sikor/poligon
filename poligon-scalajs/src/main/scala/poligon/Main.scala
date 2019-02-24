package poligon

import monix.execution.Scheduler
import org.scalajs.dom
import poligon.exampleapp.services._
import poligon.exampleapp.view.MainView
import poligon.polyproperty.PropertyObserver
import poligon.scalajscomp.ScalaJsCompFamily

object Main {

  def main(args: Array[String]): Unit = {
    val executeTasksService = new ExecuteTasksService(Scheduler.Implicits.global)
    val currentTimeService = new CurrentTimeService
    val dmService = new DmService
    val services = new Services(new FutureTranslator, executeTasksService, dmService, currentTimeService, Scheduler.Implicits.global)
    val propertyObservers = PropertyObserver.createRoot(t => t.runAsync(Scheduler.Implicits.global))
    MainView.create(services)
      .createComponent(ScalaJsCompFamily)
      .foreach { c =>
        val view = c.bind(propertyObservers)
        dom.document.body.appendChild(view)
      }(monix.execution.Scheduler.Implicits.global)
  }
}
