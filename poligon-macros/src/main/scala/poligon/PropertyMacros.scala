package poligon

import com.avsystem.commons.macros.MacroCommons

import scala.reflect.macros.blackbox

class PropertyMacros(val c: blackbox.Context) extends MacroCommons {

  import c.universe._

  def getField[T: c.WeakTypeTag](f: Tree): Tree = {
    val q"_.$getterName" = f
    val getterNameString = getterName match {
      case t: TermName => t.toString()
      case _ => abort(s"$getterName is not case class field")
    }
    val prefix = c.prefix.tree
    val resultTpe = weakTypeOf[T]
    q"$prefix.internalGetField[$resultTpe]($getterNameString)"
  }

}
