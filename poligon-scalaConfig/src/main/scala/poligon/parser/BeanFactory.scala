package poligon.parser

import java.lang.reflect.Modifier

import poligon.parser.BeanDef.{Arg, Constructor, FactoryMethod, ListValue, MapValue, PropertyValue, Referenced, SimpleValue}

import scala.collection.mutable
import scala.util.control.NonFatal

//TODO: Potrzebujemy wiecej informacji w BeanDef: Dokladnie jakiego typu jest lista, mapa, propertyValue, dokładnie, którego konstruktora użyć
// - dodać informację o typie docelowym w argumentach factory i constructor beanów. Dodać tworzoną klasę do każdego bean'a - nie tylo factory i constructor
object BeanFactory {


  private def parametersMatch(targetParameters: Array[Class[_]], args: Vector[Arg]): Boolean = {
    if (targetParameters.length == args.size) {
      targetParameters.zip(args).forall {
        case (constTpe, Arg(_, argBeanDef)) => canBeAssignedFrom(constTpe, argBeanDef)
      }
    } else {
      false
    }
  }

  private def canBeAssignedFrom(targetTpe: Class[_], beanDef: BeanDef[_]): Boolean =
    targetTpe.isAssignableFrom(beanDef.cls)


  class CreationContext(val beanDefs: Map[String, BeanDef[_]],
                        val properties: Map[String, String],
                        val createdInstances: mutable.Map[String, Any],
                        val instancesInProgress: mutable.Set[String])

  def createBeans(beanDefs: Map[String, BeanDef[_]], properties: Map[String, String]): Map[String, Any] = {
    val context = new CreationContext(beanDefs, properties, mutable.Map.empty, mutable.Set.empty)
    beanDefs.foreach {
      case (name, bdef) =>
        if (!context.createdInstances.contains(name)) {
          context.instancesInProgress += name
          val i = getOrCreateInstance(bdef, context)
          context.instancesInProgress -= name
          context.createdInstances += name -> i
        }
    }
    context.createdInstances.toMap
  }

  def createBean[T](beanDef: BeanDef[T], properties: Map[String, String]): T =
    getOrCreateInstance(beanDef, new CreationContext(Map.empty, properties, mutable.Map.empty, mutable.Set.empty))

  def getOrCreateInstance[T](beanDef: BeanDef[T], context: CreationContext): T = try {
    beanDef match {
      case Constructor(clsObj, args, setters) =>
        val instance: T = clsObj.getConstructors.filter { c =>
          parametersMatch(c.getParameterTypes, args)
        }.head.newInstance(args.map(a => getOrCreateInstance(a.value, context).asInstanceOf[AnyRef]): _*).asInstanceOf[T]
        setters.foreach { s =>
          clsObj.getMethods
            .filter(m => m.getName == s.name && parametersMatch(m.getParameterTypes, Vector(s))).head
            .invoke(instance, getOrCreateInstance(s.value, context).asInstanceOf[AnyRef])
        }
        instance
      case FactoryMethod(_, clsName, methodName, args) =>
        val cls = Class.forName(clsName)
        val method = cls.getMethods.filter(m => m.getName == methodName && parametersMatch(m.getParameterTypes, args))
          .head
        require(Modifier.isStatic(method.getModifiers), s"Factory method must be static but got: $method")
        require(method.getParameterCount == args.size, s"Expected: $method, got $args")
        method.invoke(null, args.map(a => getOrCreateInstance(a.value, context).asInstanceOf[AnyRef]): _*).asInstanceOf[T]
      case l@ListValue(cls, values) =>
        val builder = l.canBuildFrom.apply()
        builder.++=(values.map(b => getOrCreateInstance(b, context)))
        builder.result().asInstanceOf[T]
      case m@MapValue(_, values) =>
        val builder = m.canBuildFrom.apply()
        builder.++=(values.iterator.map { case (k, v) =>
          (getOrCreateInstance(k, context), getOrCreateInstance(v, context))
        })
        builder.result().asInstanceOf[T]
      case p@PropertyValue(cls, name) =>
        p.converter.convert(context.properties(name))
      case Referenced(_, name, _) =>
        if (context.createdInstances.contains(name)) {
          context.createdInstances(name).asInstanceOf[T]
        } else if (context.instancesInProgress.contains(name)) {
          throw new Exception(s"Failed to create bean: $beanDef. It is already in progress: ${context.instancesInProgress}")
        } else {
          context.instancesInProgress += name
          val i = getOrCreateInstance(context.beanDefs(name), context).asInstanceOf[T]
          context.instancesInProgress -= name
          context.createdInstances += name -> i
          i
        }
      case SimpleValue(_, value) =>
        value
    }
  } catch {
    case NonFatal(e) =>
      throw new Exception(s"Failed to create bean: $beanDef", e)
  }


}
