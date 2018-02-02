package poligon


object ReflectTest {

  val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe

  import universe._

  import scala.tools.reflect.ToolBox

  def main(args: Array[String]): Unit = {

    val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

    val t = q"1"
    val tt = tb.typecheck(t)
    println(tt.tpe)

    val s = q""" "ala" """
    val st = tb.typecheck(s)
    println(st.tpe)
    println("ala".getClass)
  }

}
