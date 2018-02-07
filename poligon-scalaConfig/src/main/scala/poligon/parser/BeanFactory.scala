package poligon.parser

import poligon.parser.BeanDef.{Arg, Constructor, FactoryMethod, ListValue, MapValue, PropertyValue, Referenced, SimpleValue}

//TODO: Potrzebujemy wiecej informacji w BeanDef: Dokladnie jakiego typu jest lista, mapa, propertyValue, dokładnie, którego konstruktora użyć
// - dodać informację o typie docelowym w argumentach factory i constructor beanów. Dodać tworzoną klasę do każdego bean'a - nie tylo factory i constructor
//TODO: Sprawdzić czy podczas konwersji do hocona nie są tracone informacje niezbędne do stworzenia beana. Kompilowalność powinna gwarantować
//poprawność hocona, np gdy wystepuje overloading constructora albo factory method to dokladnie adnotować typy.
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

  def getOrCreateInstance[T](beanDef: BeanDef[T], context: Map[String, Any]): T = beanDef match {
    case Constructor(clsObj, args, setters) =>
      val instance: T = clsObj.getConstructors.filter { c =>
        parametersMatch(c.getParameterTypes, args)
      }.head.newInstance(args.map(a => getOrCreateInstance(a.value, context))).asInstanceOf[T]
      setters.foreach { s =>
        clsObj.getMethods
          .filter(m => m.getName == s.name && parametersMatch(m.getParameterTypes, Vector(s))).head
          .invoke(instance, getOrCreateInstance(s.value, context).asInstanceOf[AnyRef])
      }
      instance
    case FactoryMethod(cls, clsName, methodName, args) =>
      cls.getMethods.filter(m => m.getName == methodName && parametersMatch(m.getParameterTypes, args)).head
        .invoke(null, args.map(a => getOrCreateInstance(a.value, context))).asInstanceOf[T]
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
    case PropertyValue(_, _) => ???
    case Referenced(_, name, _) =>
      context(name).asInstanceOf[T]
    case SimpleValue(_, value) =>
      value
  }
}
