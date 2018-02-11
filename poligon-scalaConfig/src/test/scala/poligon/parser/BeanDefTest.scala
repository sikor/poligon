package poligon.parser

import org.scalatest.FunSuite
import poligon.parser.BeanDef._
import poligon.parser.examples.{ExampleConfig, FastProcessing, ImportantService, Strategy}

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
    val beanDefs = BeanDef.toBeanDefs(ExampleConfig)
    val beans = BeanFactory.createBeans(beanDefs, Map.empty)
    assert(beans.size == 5)
  }

}
