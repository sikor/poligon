package poligon.parser

import org.scalatest.FunSuite
import poligon.parser.BeanDef._
import poligon.parser.examples.{ExampleConfig, FastProcessing, ImportantService, Strategy}

import scala.concurrent.duration.Duration

/**
  * <pre>
  * TODO:
  * - Handling .conf properties - done for strings
  * - Factory for beans, without lazy vals, keep instances in Map referenced by name.
  * - Handling default arguments
  * - Compilation should fail for construction of inner classes - done
  * - Try to scope BeanDefs to single object using type members.
  * - Support reordered constructor arguments
  * - Default value when property value not found
  * </pre>
  */
class BeanDefTest extends FunSuite {

  test("creates bean def based on constructor") {
    val importantServiceDef = new ImportantService(
      10,
      "biedronka",
      "services.customerName".toProp[String].inline,
      Strategy(
        poligon.parser.examples.FastProcessing.get
      )
    ).toBeanDef
    assert(importantServiceDef.isInstanceOf[Constructor[ImportantService]])
    val ConstructorDef = importantServiceDef.asInstanceOf[Constructor[ImportantService]]
    assert(ConstructorDef.cls == classOf[ImportantService])
    val impService = BeanFactory.createBean(importantServiceDef, Map("services.customerName" -> "RRR"))
    assert(impService.customerName == "RRR" &&
      impService.id == 10 &&
      impService.name == "biedronka" &&
      impService.strategy == Strategy(FastProcessing)
    )
  }

  test("Creates beans map based on object") {
    import BeanFactory._
    val beanDefs = BeanDef.toBeanDefs(ExampleConfig)
    val beans = BeanFactory.createBeans(beanDefs, Map("prop.service1CustomerName" -> "wlodek"))
    implicit val instances: BeanInstances = BeanInstances(beans)
    val service1 = ExampleConfig.importantService1.instance
    val strategy = ExampleConfig.strategy.instance
    assert(service1.id == 10 && service1.name == "important"
      && service1.customerName == "wlodek"
      && (service1.strategy eq strategy) && service1.duration == Duration("10s"))

    assert(ExampleConfig.namesList.instance == List("kate", "john"))
    assert(ExampleConfig.intNames.instance == Map(1 -> "jeden", 2 -> "dwa"))
    val initDataInjector = ExampleConfig.initDataInjector.instance
    assert(initDataInjector.service1 == ExampleConfig.importantService3.instance)
    assert(initDataInjector.service2 == ExampleConfig.importantService4.instance)
    assert(initDataInjector.menu == List("item1", "item2", "item3", "item4"))

  }

}
