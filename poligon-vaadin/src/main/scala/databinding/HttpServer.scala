package databinding

import com.avsystem.commons.misc.Opt
import com.typesafe.scalalogging.StrictLogging
import com.vaadin.annotations.{Push, Theme}
import com.vaadin.server._
import com.vaadin.ui.UI
import javax.servlet.{ServletConfig, ServletException}
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.session.{DefaultSessionIdManager, SessionHandler}
import org.eclipse.jetty.servlet.{ServletContextHandler, ServletHolder}
import poligon.polyproperty.PropertyObserver.PropertyObservers

import scala.concurrent.ExecutionContextExecutor

/**
  * Next:
  * - two nested menus
  * - refreshing all values without rebuilding panel, refresh button above the objects panel (parent of presenter calls it)
  * - server push imitation using refresher
  */
object HttpServer {

  @Theme("valo")
  @Push
  class PoligonUI(executeTasksService: ExecuteTasksService) extends UI with StrictLogging {

    implicit object UIExecutor extends ExecutionContextExecutor {
      override def reportFailure(cause: Throwable): Unit = logger.error("Runnable failed", cause)

      override def execute(command: Runnable): Unit = {
        access(command)
      }
    }

    override def init(request: VaadinRequest): Unit = {
      implicit val po: PropertyObservers = new PropertyObservers(Opt.Empty)
      val presenter = new MainViewPresenter(new ExecuteTasksPresenter(executeTasksService))
      val view = DefaultViewFactory.createView(presenter).bind(po)
      setContent(view)
    }
  }

  def main(args: Array[String]): Unit = {
    val executeTasksService = new ExecuteTasksService
    val server = new Server(8080)
    val servletContextHandler = new ServletContextHandler()
    servletContextHandler.setContextPath("/*")
    servletContextHandler.addServlet(new ServletHolder(new PoligonServlet(() => new PoligonUI(executeTasksService), classOf[PoligonUI])), "/*")
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
