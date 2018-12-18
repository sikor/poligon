package databinding.properties

import java.util
import java.util.Locale

import com.github.ghik.silencer.silent
import com.vaadin.server._
import com.vaadin.shared.communication.{ServerRpc, SharedState}
import com.vaadin.ui.declarative.DesignContext
import com.vaadin.ui.{Component, HasComponents, UI}
import elemental.json.JsonObject
import org.jsoup.nodes.Element
import poligon.polyproperty.PropertyObserver.PropertyObservers

object DynamicComponent {
  def dispose(component: Component): Unit = component match {
    case d: DynamicComponent =>
      d.dispose()
    case _ =>
  }
}

class DynamicComponent(private val _wrapped: Component, private val listeners: PropertyObservers) extends Component {

  private def wrapped: Component = _wrapped

  def dispose(): Unit = listeners.dispose()

  override def getStyleName: String = wrapped.getStyleName

  override def setStyleName(style: String): Unit = wrapped.setStyleName(style)

  override def addStyleName(style: String): Unit = wrapped.addStyleName(style)

  override def removeStyleName(style: String): Unit = wrapped.removeStyleName(style)

  override def getPrimaryStyleName: String = wrapped.getPrimaryStyleName

  override def setPrimaryStyleName(style: String): Unit = wrapped.setPrimaryStyleName(style)

  override def isEnabled: Boolean = wrapped.isEnabled

  override def setEnabled(enabled: Boolean): Unit = wrapped.setEnabled(enabled)

  override def isVisible: Boolean = wrapped.isVisible

  override def setVisible(visible: Boolean): Unit = wrapped.setVisible(visible)

  override def setParent(parent: HasComponents): Unit = wrapped.setParent(parent)

  override def getParent: HasComponents = wrapped.getParent

  override def isReadOnly: Boolean = wrapped.isReadOnly

  override def setReadOnly(readOnly: Boolean): Unit = wrapped.setReadOnly(readOnly)

  override def getCaption: String = wrapped.getCaption

  override def setCaption(caption: String): Unit = wrapped.setCaption(caption)

  override def getIcon: Resource = wrapped.getIcon

  override def setIcon(icon: Resource): Unit = wrapped.setIcon(icon)

  override def getUI: UI = wrapped.getUI

  override def attach(): Unit = wrapped.attach()

  override def getLocale: Locale = wrapped.getLocale

  override def setId(id: String): Unit = wrapped.setId(id)

  override def getId: String = wrapped.getId

  override def getDescription: String = wrapped.getDescription

  override def readDesign(design: Element, designContext: DesignContext): Unit = wrapped.readDesign(design, designContext)

  override def writeDesign(design: Element, designContext: DesignContext): Unit = wrapped.writeDesign(design, designContext)

  override def addListener(listener: Component.Listener): Unit = wrapped.addListener(listener)

  override def removeListener(listener: Component.Listener): Unit = wrapped.removeListener(listener)

  override def addAttachListener(listener: ClientConnector.AttachListener): Unit = wrapped.addAttachListener(listener)

  override def removeAttachListener(listener: ClientConnector.AttachListener): Unit = wrapped.removeAttachListener(listener)

  override def addDetachListener(listener: ClientConnector.DetachListener): Unit = wrapped.addDetachListener(listener)

  override def removeDetachListener(listener: ClientConnector.DetachListener): Unit = wrapped.removeDetachListener(listener)

  override def retrievePendingRpcCalls(): util.List[ClientMethodInvocation] = wrapped.retrievePendingRpcCalls()

  override def isConnectorEnabled: Boolean = wrapped.isConnectorEnabled

  override def getStateType: Class[_ <: SharedState] = wrapped.getStateType

  @silent
  override def requestRepaint(): Unit = wrapped.requestRepaint()

  override def markAsDirty(): Unit = wrapped.markAsDirty()

  @silent
  override def requestRepaintAll(): Unit = wrapped.requestRepaintAll()

  override def markAsDirtyRecursive(): Unit = wrapped.markAsDirtyRecursive()

  override def isAttached: Boolean = wrapped.isAttached

  override def detach(): Unit = wrapped.detach()

  override def getExtensions: util.Collection[Extension] = wrapped.getExtensions

  override def removeExtension(extension: Extension): Unit = wrapped.removeExtension(extension)

  override def beforeClientResponse(initial: Boolean): Unit = wrapped.beforeClientResponse(initial)

  override def encodeState(): JsonObject = wrapped.encodeState()

  override def handleConnectorRequest(request: VaadinRequest, response: VaadinResponse, path: String): Boolean = wrapped.handleConnectorRequest(request, response, path)

  override def getRpcManager(rpcInterfaceName: String): ServerRpcManager[_ <: ServerRpc] = wrapped.getRpcManager(rpcInterfaceName)

  override def getErrorHandler: ErrorHandler = wrapped.getErrorHandler

  override def setErrorHandler(errorHandler: ErrorHandler): Unit = wrapped.setErrorHandler(errorHandler)

  override def getWidth: Float = wrapped.getWidth

  override def getHeight: Float = wrapped.getHeight

  override def getWidthUnits: Sizeable.Unit = wrapped.getWidthUnits

  override def getHeightUnits: Sizeable.Unit = wrapped.getHeightUnits

  override def setHeight(height: String): Unit = wrapped.setHeight(height)

  override def setWidth(width: Float, unit: Sizeable.Unit): Unit = wrapped.setWidth(width, unit)

  override def setHeight(height: Float, unit: Sizeable.Unit): Unit = wrapped.setHeight(height, unit)

  override def setWidth(width: String): Unit = wrapped.setWidth(width)

  override def setSizeFull(): Unit = wrapped.setSizeFull()

  override def setSizeUndefined(): Unit = wrapped.setSizeUndefined()

  override def setWidthUndefined(): Unit = wrapped.setWidthUndefined()

  override def setHeightUndefined(): Unit = wrapped.setHeightUndefined()

  override def getConnectorId: String = wrapped.getConnectorId
}
