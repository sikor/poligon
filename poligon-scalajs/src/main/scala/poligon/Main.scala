package poligon

import monix.execution.Scheduler
import org.scalajs.dom
import poligon.exampleapp.services._
import poligon.exampleapp.view.MainView
import poligon.polyproperty.PropertyObserver
import poligon.scalajscomp.ScalaJsCompFactory

object Main {

  def main(args: Array[String]): Unit = {
    val executeTasksService = new ExecuteTasksService(Scheduler.Implicits.global)
    val currentTimeService = new CurrentTimeService
    val dmService = new DmService
    val services = new Services(new FutureTranslator, executeTasksService, dmService, currentTimeService, Scheduler.Implicits.global)
    val propertyObservers = PropertyObserver.createRoot
    val view = MainView.create(services)
      .createComponent(ScalaJsCompFactory)
      .bind(propertyObservers)
    dom.document.body.appendChild(view)
  }
}
