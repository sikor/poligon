package poligon.parser

import com.avsystem.commons.spring.HoconBeanDefinitionReader
import com.typesafe.config.{ConfigFactory, ConfigResolveOptions}
import org.scalatest.FunSuite
import org.springframework.beans.factory.support.DefaultListableBeanFactory
import org.springframework.context.support.GenericApplicationContext
import org.springframework.core.DefaultParameterNameDiscoverer
import poligon.parser.BeanDef._
import poligon.parser.examples._

import scala.concurrent.duration.Duration

/**
  * <pre>
  * TODO:
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

    assert(ExampleConfig.mapAndList.instance.namesList == List("kate", "john"))
    assert(ExampleConfig.mapAndList.instance.intNames.keys == Set(1, 2))
    val initDataInjector = ExampleConfig.initDataInjector.instance
    assert(initDataInjector.service1 == ExampleConfig.importantService3.instance)
    assert(initDataInjector.service2 == ExampleConfig.importantService4.instance)
    assert(initDataInjector.menu == List("item1", "item2", "item3", "item4"))
  }

  test("Generates hocon config") {
    val beanDefs = BeanDef.toBeanDefs(ExampleConfig)
    val h = HoconPrinter.toHocon(beanDefs)
    val propsStr =
      """
        |prop.service1CustomerName = "wlodek"
      """.stripMargin

    val props = ConfigFactory.parseString(propsStr)
    println(props)
    println(h)
    val c = ConfigFactory.parseString(h).withFallback(props).resolve(ConfigResolveOptions.noSystem)
    println(c)

    val pnd = new DefaultParameterNameDiscoverer
    pnd.addDiscoverer(new ScalaReflectionParameterNameDiscoverer)
    val beanFactory = new DefaultListableBeanFactory
    beanFactory.setParameterNameDiscoverer(pnd)
    val context = new GenericApplicationContext(beanFactory)
    context.setAllowCircularReferences(false)
    new HoconBeanDefinitionReader(context).loadBeanDefinitions(c)
    context.refresh()
    println(context.getBeanDefinitionCount)

    val s1 = context.getBean("importantService1").asInstanceOf[ImportantService]
    assert(s1.id == 10 && s1.name == "important")
  }

}
