package poligon


object ReflectTest {

  def get: ReflectTest.type = this

  val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe

  class IntInMethod {
    var value: Int = 0

    def setValue(i: Int): Unit = value = i
  }

  import universe._

  import scala.tools.reflect.ToolBox

  def main(args: Array[String]): Unit = {
    val c = new IntInMethod
    c.getClass.getMethod("setValue", classOf[Int]).invoke(c, 12.asInstanceOf[AnyRef])
    println(c.value)

    val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

    val t = q"poligon.ReflectTest.get"
    val tt = tb.typecheck(t)
    val ttt = tt.tpe
    tt.symbol.isModuleClass
    println(tt.symbol.isModuleClass)
    println(tt.tpe.typeSymbol.isModuleClass)
    println(tt.tpe.typeSymbol.isModule)
    println(tt.tpe.typeSymbol.fullName)

    ReflectTest.get
    val s = q""" "ala" """
    val st = tb.typecheck(s)
    println(st.tpe)
    println("ala".getClass)


    println(ReflectTest.getClass)
  }

}
