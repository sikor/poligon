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

class InitDataInjector(val menu: List[String]) {

  @BeanProperty
  var service1: ImportantService = _
  @BeanProperty
  var service2: ImportantService = _
}

trait GuiModule {

  def initDataInjectorEP(initDataInjector: Constructor[InitDataInjector]): Constructor[InitDataInjector] =
    initDataInjector

  def menuEP(menu: ListValue[String, List]): ListValue[String, List] = menu

  def menu: ListValue[String, List] = menuEP(List("item1", "item2").toListValue)

  def initDataInjector: Constructor[InitDataInjector] = initDataInjectorEP(new InitDataInjector(menu.inline).toConstructorValue)
}

trait OptionalServiceModule1 extends GuiModule {

  def importantService3: BeanDef[ImportantService] =
    new ImportantService(12, "withReference", "ziom", Strategy(FastProcessing.get)).toBeanDef

  override def initDataInjectorEP(initDataInjector: Constructor[InitDataInjector]): Constructor[InitDataInjector] =
    super.initDataInjectorEP(initDataInjector).withSetters(_.setService1(importantService3.ref))

  override def menuEP(menu: ListValue[String, List]): ListValue[String, List] =
    super.menuEP(menu).amend(List("item3").toListValue)

}

trait OptionalServiceModule2 extends StrategyModule with GuiModule {

  def importantService4: BeanDef[ImportantService] =
    new ImportantService(13, "inline", "ziom", strategy.inline).toBeanDef

  override def initDataInjectorEP(initDataInjector: Constructor[InitDataInjector]): Constructor[InitDataInjector] =
    super.initDataInjectorEP(initDataInjector).withSetters(_.setService2(importantService4.ref))

  override def menuEP(menu: ListValue[String, List]): ListValue[String, List] =
    super.menuEP(menu).amend(List("item4").toListValue)


}

trait StrategyModule {
  def strategy: BeanDef[Strategy] =
    Strategy(FastProcessing.get).toBeanDef
}

object ExampleConfig extends StrategyModule with GuiModule with OptionalServiceModule1 with OptionalServiceModule2 {

  def intNames: MapValue[Int, String, Map] = Map(1 -> "jeden", 2 -> "dwa").toMapValue

  def namesList: ListValue[String, List] = List("kate", "john").toListValue

  def importantService1: BeanDef[ImportantService] =
    new ImportantService(10, "important", "prop.service1CustomerName".toProp[String].inline, strategy.ref)
      .toConstructorValue
      .withSetters(_.setDuration(Duration("10s")))

  def importantService2: BeanDef[ImportantService] =
    new ImportantService(11, "withReference", "ziom", strategy.ref).toBeanDef

}