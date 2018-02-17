package poligon.parser.examples

import poligon.parser.BeanDef
import poligon.parser.BeanDef._

import scala.beans.BeanProperty
import scala.concurrent.duration.Duration

sealed trait ProcessingType {
  def get: this.type = this
}

case object FastProcessing extends ProcessingType

case object PreciseProcessing extends ProcessingType

case class Strategy(processingTpe: ProcessingType)

class ImportantService(val id: Int, val name: String, val customerName: String, val strategy: Strategy) {

  @BeanProperty
  var duration: Duration = _

}


object ExampleConfig {

  def intNames: MapValue[Int, String, Map] = Map(1 -> "jeden", 2 -> "dwa").toMapValue

  def namesList: ListValue[String, List] = List("kate", "john").toListValue

  def importantService1: BeanDef[ImportantService] =
    new ImportantService(10, "important", "wlodek", strategy.ref).toConstructorValue
      .withSetters(_.setDuration(Duration("10s")))

  def strategy: BeanDef[Strategy] =
    Strategy(FastProcessing.get).toBeanDef

  def importantService2: BeanDef[ImportantService] =
    new ImportantService(11, "withReference", "ziom", strategy.ref).toBeanDef

  def importantService3: BeanDef[ImportantService] =
    new ImportantService(11, "withReference", "ziom", Strategy(FastProcessing.get)).toBeanDef

  def importantService4: BeanDef[ImportantService] =
    new ImportantService(11, "withReference", "ziom", strategy.inline).toBeanDef
}