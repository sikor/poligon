import java.nio.charset.Charset
import java.util

import MyMacros._
import com.avsystem.commons.SharedExtensions._

object Main {
  def main(args: Array[String]): Unit = {
    val s = new String(Array(1.toChar,1.toChar,2.toChar).asInstanceOf[Array[Byte]], 10, 12, Charset.defaultCharset()).toHoconConfig
    println(s)
  }
}
