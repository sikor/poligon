package poligon

import com.avsystem.commons.concurrent.RunNowEC
import poligon.parser.BeanDef
import poligon.parser.BeanDef.{BeanDef, ListValue}
import somePackage.JavaClass
import poligon.parser.BeanDef._

import scala.concurrent.ExecutionContext

class TakesList(val vec: Vector[Int])

class ScalaNormalClass(val arg1: String, arg2: Int)(implicit ec: ExecutionContext)

class MainClass(val normalClass: ScalaNormalClass, val normalClazz: ScalaNormalClass)

trait PartialConfig {
  def par: BeanDef[ScalaNormalClass]
}

trait HasStaticMethods {
  def factory(someArg: Int) = "StaticMethodFactory"
}

object InheritsStaticMethod extends HasStaticMethods {
  override def factory(someArg: Int) = "StaticMethodFactory"
}

trait DefaultConfig {
  this: PartialConfig =>

  def foo: BeanDef[ScalaNormalClass] = new ScalaNormalClass(InheritsStaticMethod.factory(32), 2)(RunNowEC.get).toBeanDef

  def bar: BeanDef[MainClass] = new MainClass(foo.ref, par.inline).toBeanDef
}

object CustomConfig extends DefaultConfig with PartialConfig {
  def par: BeanDef[ScalaNormalClass] = new ScalaNormalClass("pardef", 3)(RunNowEC.get).toBeanDef

  def some(arg: ListValue[Int, List]): BeanDef[TakesList] = new TakesList(List(1, 2).toListDef.amend(arg).as[Vector]).toBeanDef

  def some: BeanDef[TakesList] = some(ListValue.empty)

  def javaFactoryBean: BeanDef[JavaClass] = JavaClass.javaFactory("javaFactoryArg").toBeanDef
}

class HasListArg(names: List[String])


object Main {

  final val s = "constant!! "
  final val map = Map(1 -> "String")

  def main(args: Array[String]): Unit = {
    //        println(new ScalaNormalClass("normal \" \\\" class \n", 23)(RunNowEC).toBeanDef)
    println(new JavaClass(s).toBeanDef)
    //    println(CustomConfig.bar)
    //    println(new HasListArg(List("pawel", "asia")).toBeanDef)
    println(CustomConfig.some(List(1, 2, 3).toListDef))
    println(BeanDef.toBeanDefs(CustomConfig).toHocon)
  }
}

/**
  * hocon functionality
  * * append to list/map, override key in map (in hocon you can copy existing bean def and change it or change the argument in exisiting bean)
  * Copying existing bean by calling constructor method and with correct arguments so you get bean def.
  * Split: abstract bean and concrete bean, for each bean that can be modificable you have to create abstract bean with method arguments and use it to create concrete bean.
  * You can override default concrete bean using method overriding. Only methods without arguments (even no empty arguments list?) are used as concret beans and are listed in final configuration.
  * (Perhaps abstract beans without argument should be denoted as empty arguments list or separate bean type)
  * * reference any existing nested value in config, by constructor argument - copy method, by getters and setters
  * * modify existing field in the bean, using existing value, for example append to list/string
  * * prototyping like hocon? - needs referencing, copying instead of overriding beans, special case for changing class
  * factory beans/methods (for scala singleton beans methods and classic java factory beans with arguments)
  * default arguments
  * Singleton objects refs
  * handling setters
  *
  * Everything should be able to have a name: List, ClassDef, FactoryDef, Map - one macro that translates some scala code to hocon.
  *
  **/