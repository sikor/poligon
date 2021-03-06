package poligon.parser

import java.lang.reflect.Modifier

import poligon.parser.BeanDef.{Constructor, FactoryMethod, ListValue, MapValue, PropertyValue, Referenced, SimpleValue}

import scala.collection.mutable
import scala.util.control.NonFatal

object BeanFactory {

  case class BeanInstances(instances: Map[String, Any]) {
    def apply[T](name: String): T = instances(name).asInstanceOf[T]
  }

  implicit class BeanImplExt[T](beanDef: BeanDef[T]) {
    def instance(implicit instances: BeanInstances): T = macro poligon.HoconConfigMacros.getBeanInstance[T]
  }


  private def parametersMatch(targetParameters: Array[Class[_]], args: Vector[BeanDef[_]]): Boolean = {
    if (targetParameters.length == args.size) {
      targetParameters.zip(args).forall {
        case (constTpe, argBeanDef) => canBeAssignedFrom(constTpe, argBeanDef)
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

  private def getOrCreateInstance[T](beanDef: BeanDef[T], context: CreationContext): T = try {
    beanDef match {
      case Constructor(clsObj, args, setters) =>
        val instance: T = clsObj.getConstructors.filter { c =>
          parametersMatch(c.getParameterTypes, args.map(_.value))
        }.head.newInstance(args.map(a => getOrCreateInstance(a.value, context).asInstanceOf[AnyRef]): _*).asInstanceOf[T]
        setters.values.foreach { s =>
          clsObj.getMethods
            .find(m => m.getName == s.name && parametersMatch(m.getParameterTypes, Vector(s.value)))
            .getOrElse(throw new Exception(s"Failed to find setter with name: ${s.name} in class: $clsObj. " +
              s"Available methods: ${clsObj.getMethods.mkString(", ")}"))
            .invoke(instance, getOrCreateInstance(s.value, context).asInstanceOf[AnyRef])
        }
        instance
      case FactoryMethod(_, clsName, methodName, args) =>
        val cls = Class.forName(clsName)
        val method = cls.getMethods.filter(m => m.getName == methodName && parametersMatch(m.getParameterTypes, args.map(_.value)))
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
