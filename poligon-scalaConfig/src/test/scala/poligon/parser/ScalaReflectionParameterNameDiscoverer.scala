package poligon.parser

import java.lang.reflect.{Constructor, Executable, Method, Modifier}

import org.springframework.core.ParameterNameDiscoverer

import scala.ref.WeakReference
import scala.reflect.api.JavaUniverse
import scala.reflect.{ScalaLongSignature, ScalaSignature}

/**
 * Author: ghik
 * Created: 10/09/15.
 */
class ScalaReflectionParameterNameDiscoverer extends ParameterNameDiscoverer {

  // we don't want to keep the universe in memory forever, so we don't use scala.reflect.runtime.universe
  private var universeRef: WeakReference[JavaUniverse] = _

  private def universe: JavaUniverse = {
    Option(universeRef).flatMap(_.get) match {
      case Some(result) => result
      case None =>
        val result = new scala.reflect.runtime.JavaUniverse
        universeRef = new WeakReference[JavaUniverse](result)
        result
    }
  }

  def isScala(cls: Class[_]) =
    Iterator.iterate[Class[_]](cls)(_.getEnclosingClass).takeWhile(_ != null)
      .exists(c => c.getAnnotation(classOf[ScalaSignature]) != null || c.getAnnotation(classOf[ScalaLongSignature]) != null)

  private def discoverNames(u: JavaUniverse)(executable: Executable, symbolPredicate: u.Symbol => Boolean): Array[String] = {
    import u._

    val declaringClass = executable.getDeclaringClass
    val mirror = runtimeMirror(declaringClass.getClassLoader)
    val ownerSymbol =
      if (Modifier.isStatic(executable.getModifiers)) mirror.moduleSymbol(declaringClass).moduleClass.asType
      else mirror.classSymbol(declaringClass)

    def argErasuresMatch(ms: MethodSymbol) =
      ms.paramLists.flatten.map(s => mirror.runtimeClass(s.typeSignature)) == executable.getParameterTypes.toList

    def paramNames(ms: MethodSymbol) =
      ms.paramLists.flatten.map(_.name.toString).toArray

    ownerSymbol.toType.members
      .find(s => symbolPredicate(s) && argErasuresMatch(s.asMethod))
      .map(s => paramNames(s.asMethod))
      .orNull
  }

  def getParameterNames(ctor: Constructor[_]): Array[String] =
    if (isScala(ctor.getDeclaringClass))
      discoverNames(universe)(ctor, s => s.isConstructor)
    else null

  def getParameterNames(method: Method): Array[String] =
    if (isScala(method.getDeclaringClass))
      discoverNames(universe)(method, s => s.isMethod && s.name.toString == method.getName)
    else null
}
