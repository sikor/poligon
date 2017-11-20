package poligon.parser

import org.scalatest.FunSuite
import poligon.parser.BeanDef._
import poligon.parser.examples.{FastProcessing, ImportantService, Strategy}

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
      "services.customerName".toProp.inline,
      Strategy(FastProcessing.get.toBeanDef.inline).toBeanDef.inline
    ).toBeanDef
    assert(importantServiceDef.isInstanceOf[Constructor[ImportantService]])
    val ConstructorDef = importantServiceDef.asInstanceOf[Constructor[ImportantService]]
    assert(getClass.getClassLoader.loadClass(ConstructorDef.clsName) == classOf[ImportantService])
  }

}
