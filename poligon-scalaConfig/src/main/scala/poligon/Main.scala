package poligon

import com.avsystem.commons.concurrent.RunNowEC
import poligon.MyMacros._
import somePackage.JavaClass

import scala.concurrent.ExecutionContext

class TakesList(val vec: Vector[Int])

class ScalaNormalClass(val arg1: String, arg2: Int)(implicit ec: ExecutionContext)

class MainClass(val normalClass: ScalaNormalClass, val normalClazz: ScalaNormalClass)

trait PartialConfig {
  def par: BeanDef[ScalaNormalClass]
}


trait DefaultConfig {
  this: PartialConfig =>

  def foo = new ScalaNormalClass("arg1", 2)(RunNowEC).toBeanDef

  def bar = new MainClass(foo.ref, par.ref).toBeanDef
}

object CustomConfig extends DefaultConfig with PartialConfig {
  def par = new ScalaNormalClass("pardef", 3)(RunNowEC).toBeanDef

  def some(arg: HoconList[Int]) = new TakesList(List(1, 2).toListDef.amend(arg).as[Vector]).toBeanDef
}

class HasListArg(names: List[String])


object Main {

  final val s = "constant!! "
  final val map = Map(1 -> "String")

  def main(args: Array[String]): Unit = {
    println(new ScalaNormalClass("normal \" \\\" class \n", 23)(RunNowEC).toBeanDef)
    println(new JavaClass(s).toBeanDef)
    println(CustomConfig.bar)
    println(new HasListArg(List("pawel", "asia")).toBeanDef)
    println(CustomConfig.some(List(1, 2, 3).toListDef))
  }
}

/**
  * hocon functionality
  * * append to list/map, override key in map
  * * reference any existing nested value in config, by constructor argument - copy method, by getters and setters
  * * modify existing field in the bean, using existing value, for example append to list/string
  * * prototyping like hocon? - needs referencing, copying instead of overriding beans, special case for changing class
  * factory beans/methods
  * default arguments
  **/