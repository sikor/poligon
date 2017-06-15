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

  def foo = new ScalaNormalClass("arg1", 2)(RunNowEC).toHoconConfig

  def bar = new MainClass(foo.ref, par.ref).toHoconConfig
}

object CustomConfig extends DefaultConfig with PartialConfig {
  def par = new ScalaNormalClass("pardef", 3)(RunNowEC).toHoconConfig
}

object Main {

  final val s = "constant!! "

  def main(args: Array[String]): Unit = {
    println(new ScalaNormalClass("normal \" \\\" class \n", 23)(RunNowEC).toHoconConfig)
    println(new JavaClass(s).toHoconConfig)
    println(CustomConfig.bar)
  }
}
