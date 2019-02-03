package poligon.exampleapp

import com.typesafe.scalalogging.StrictLogging
import com.vaadin.annotations.{Push, Theme}
import com.vaadin.server._
import com.vaadin.ui.UI
import javax.servlet.{ServletConfig, ServletException}
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.session.{DefaultSessionIdManager, SessionHandler}
import org.eclipse.jetty.servlet.{ServletContextHandler, ServletHolder}
import poligon.exampleapp.services.{DmService, ExecuteTasksService}
import poligon.exampleapp.view.ExecuteTasksButton.ExecuteTasksContext
import poligon.exampleapp.view.MainView
import poligon.exampleapp.view.MainView.MainViewContext
import poligon.polyproperty.PropertyObserver

import scala.concurrent.ExecutionContextExecutor

/**
  * Next:
  * - Rewrite ExecuteTasks to Comp:
  * -- Async service should be easily callable from View and model updated
  * -- Situation when view is not present anymore when data arrive should be somehow handled
  * -- Situation when calling service from one place also updates other fragments of UI that rely on these data
  * -- View should not be blocked while waiting for data
  * -- Also consider registering on async push data (more generalised case)
  * - Implement task service to force objects view to refresh and handle async commands
  */
object HttpServer {

  @Theme("valo")
  @Push
  class PoligonUI(executeTasksService: ExecuteTasksService, dmService: DmService) extends UI with StrictLogging {

    implicit object UIExecutor extends ExecutionContextExecutor {
      override def reportFailure(cause: Throwable): Unit = logger.error("Runnable failed", cause)

      override def execute(command: Runnable): Unit = {
        access(command)
      }
    }

    override def init(request: VaadinRequest): Unit = {
      val propertyObservers = PropertyObserver.createRoot
      val presenter = new MainViewContext(new ExecuteTasksContext(executeTasksService), dmService)
      val view = MainView.create(presenter).bind(propertyObservers)
      setContent(view)
    }
  }

  def main(args: Array[String]): Unit = {
    val executeTasksService = new ExecuteTasksService
    val dmService = new DmService
    val server = new Server(8080)
    val servletContextHandler = new ServletContextHandler()
    servletContextHandler.setContextPath("/*")
    servletContextHandler.addServlet(new ServletHolder(new PoligonServlet(() => new PoligonUI(executeTasksService, dmService), classOf[PoligonUI])), "/*")
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
