import java.nio.charset.Charset

import MyMacros._
import com.avsystem.commons.concurrent.RunNowEC

import scala.concurrent.ExecutionContext

class ScalaNormalClass(val arg1: String, arg2: Int)(implicit ec: ExecutionContext)

object Main {

  final val s = "constant!! "

  def main(args: Array[String]): Unit = {
    println(new ScalaNormalClass("normal \" \\\" class \n", 23)(RunNowEC).toHoconConfig)
    println(new JavaClass(s).toHoconConfig)
  }
}
