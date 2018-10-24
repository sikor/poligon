package databinding

import com.vaadin.server._
import com.vaadin.ui.UI
import javax.servlet.{ServletConfig, ServletException}
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.session.{DefaultSessionIdManager, SessionHandler}
import org.eclipse.jetty.servlet.{ServletContextHandler, ServletHolder}

/**
  * Next:
  * - two nested menus
  * - refreshing all values without rebuilding panel, refresh button above the objects panel (parent of presenter calls it)
  * - server push imitation using refresher
  */
object HttpServer {

  class VaadinUI extends UI {
    override def init(request: VaadinRequest): Unit = {
      val presenter = new MainViewPresenter
      val view = DefaultViewFactory.createView(presenter)
      setContent(view)
    }
  }

  def main(args: Array[String]): Unit = {
    val server = new Server(8080)
    val servletContextHandler = new ServletContextHandler()
    servletContextHandler.setContextPath("/*")
    servletContextHandler.addServlet(new ServletHolder(new HelloServlet(() => new VaadinUI, classOf[VaadinUI])), "/*")
    val idManager = new DefaultSessionIdManager(server)
    server.setSessionIdManager(idManager)
    val sessionHandler = new SessionHandler
    sessionHandler.setHandler(servletContextHandler)
    server.setHandler(sessionHandler)
    server.start()
    server.join()
  }

  class HelloServlet(ui: () => UI, uiClass: Class[_ <: UI]) extends VaadinServlet {

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
