package tests

import com.avsystem.commons.concurrent.RunNowEC
import poligon.BeanDef
import poligon.MyMacros._
import somePackage.JavaClass

import scala.concurrent.ExecutionContext

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
  }
}
