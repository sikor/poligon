package poligon

import monix.execution.Scheduler
import org.scalajs.dom
import poligon.comp.BindableComp
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
    val services = new Services(new FakeTranslator, executeTasksService, dmService, currentTimeService)
    val runner = new TaskRunner(Scheduler.Implicits.global)
    val propertyObservers = PropertyObserver.createRoot(runner, services)
    val comp = MainView.create(services)
      .createComponent(ScalaJsCompFamily)
    val view = BindableComp.bind(comp, propertyObservers)
      .foreachL { c =>
        dom.document.body.appendChild(c)
      }
    runner.renderMainView(view)
  }
}
