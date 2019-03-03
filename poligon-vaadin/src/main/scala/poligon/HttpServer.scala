package poligon

import java.util
import java.util.concurrent.{AbstractExecutorService, TimeUnit}

import com.typesafe.scalalogging.StrictLogging
import com.vaadin.annotations.{Push, Theme}
import com.vaadin.server._
import com.vaadin.ui.UI
import javax.servlet.{ServletConfig, ServletException}
import monix.execution.ExecutionModel
import monix.execution.schedulers.ExecutorScheduler
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.session.{DefaultSessionIdManager, SessionHandler}
import org.eclipse.jetty.servlet.{ServletContextHandler, ServletHolder}
import poligon.comp.BindableComp
import poligon.exampleapp.services._
import poligon.exampleapp.view.MainView
import poligon.polyproperty.PropertyObserver
import poligon.polyproperty.PropertyObserver.TaskRunner
import poligon.vaadincomp.VaadinCompFamily

import scala.concurrent.ExecutionContextExecutorService

/**
  * Next:
  * - Rewrite ExecuteTasks to Comp:
  * -- Async service should be easily callable from View and model updated
  * -- Situation when view is not present anymore when data arrive should be somehow handled
  * -- Situation when calling service from one place also updates other fragments of UI that rely on these data
  * -- View should not be blocked while waiting for data
  * -- Also consider registering on async push data (more generalised case)
  * - Implement current time service to present handling event registration.
  * - Implement task service to force objects view to refresh and handle async commands
  */
object HttpServer {

  @Theme("valo")
  @Push
  class PoligonUI(dmService: DmService,
                  currentTimeService: CurrentTimeService) extends UI with StrictLogging {

    private object UIExecutor extends AbstractExecutorService with ExecutionContextExecutorService {
      override def reportFailure(cause: Throwable): Unit = logger.error("Runnable failed", cause)

      override def execute(command: Runnable): Unit = {
        access(command)
      }

      def shutdown(): Unit = throw new NotImplementedError()


      def shutdownNow(): util.List[Runnable] = throw new NotImplementedError()

      def isShutdown: Boolean = false

      def isTerminated: Boolean = false

      def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = throw new NotImplementedError()
    }

    private val monixScheduler: ExecutorScheduler =
      ExecutorScheduler(UIExecutor, (ex: Throwable) => UIExecutor.reportFailure(ex), ExecutionModel.Default)

    override def init(request: VaadinRequest): Unit = {
      val executeTasksService = new ExecuteTasksService()
      val services = new Services(new FakeTranslator, executeTasksService, dmService, currentTimeService)
      val taskRunner = new TaskRunner(monixScheduler, fail => logger.error("Failed to run task", fail))
      val propertyObservers = PropertyObserver.createRoot(taskRunner, services)
      val comp = MainView.create(services).createComponent(VaadinCompFamily)
      val mainView = BindableComp.bind(comp, propertyObservers)
        .foreachL { view =>
          setContent(view)
        }
      taskRunner.renderMainView(mainView)
    }
  }

  def main(args: Array[String]): Unit = {
    val currentTimeService = new CurrentTimeService
    val dmService = new DmService
    val server = new Server(8080)
    val servletContextHandler = new ServletContextHandler()
    servletContextHandler.setContextPath("/*")
    servletContextHandler.addServlet(new ServletHolder(new PoligonServlet(() =>
      new PoligonUI(dmService, currentTimeService), classOf[PoligonUI])), "/*")
    val idManager = new DefaultSessionIdManager(server)
    server.setSessionIdManager(idManager)
    val sessionHandler = new SessionHandler
    sessionHandler.setHandler(servletContextHandler)
    server.setHandler(sessionHandler)
    server.start()
    server.join()
  }

  class PoligonServlet(ui: () => UI, uiClass: Class[_ <: UI]) extends VaadinServlet {

    @throws[ServletException]
    override def init(servletConfig: ServletConfig): Unit = {
      super.init(servletConfig)
      getService.addSessionInitListener((event: SessionInitEvent) => {
        event.getSession.addUIProvider(new UIProvider {
          override def getUIClass(event: UIClassSelectionEvent): Class[_ <: UI] = uiClass

          override def createInstance(event: UICreateEvent): UI = ui()
        })
      })
    }
  }

}
