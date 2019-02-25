package poligon

import monix.execution.Scheduler
import org.scalajs.dom
import poligon.exampleapp.services._
import poligon.exampleapp.view.MainView
import poligon.polyproperty.PropertyObserver
import poligon.polyproperty.PropertyObserver.TaskRunner
import poligon.scalajscomp.ScalaJsCompFamily

object Main {

  def main(args: Array[String]): Unit = {
    val executeTasksService = new ExecuteTasksService
    val currentTimeService = new CurrentTimeService
    val dmService = new DmService
    val services = new Services(new FutureTranslator, executeTasksService, dmService, currentTimeService)
    val runner = new TaskRunner(Scheduler.Implicits.global)
    val propertyObservers = PropertyObserver.createRoot(runner)
    val view = MainView.create(services)
      .createComponent(ScalaJsCompFamily)
      .bind(propertyObservers)
      .foreachL { c =>
        dom.document.body.appendChild(c)
      }
    runner.runTask(view)
  }
}
