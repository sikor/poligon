package poligon.parser

import poligon.parser.BeanDef.{Arg, Constructor, FactoryMethod, ListValue, MapValue, PropertyValue, Referenced, SimpleValue}

//TODO: Potrzebujemy wiecej informacji w BeanDef: Dokladnie jakiego typu jest lista, mapa, propertyValue, dokładnie, którego konstruktora użyć
// - dodać informację o typie docelowym w argumentach factory i constructor beanów. Dodać tworzoną klasę do każdego bean'a - nie tylo factory i constructor
//TODO: Sprawdzić czy podczas konwersji do hocona nie są tracone informacje niezbędne do stworzenia beana. Kompilowalność powinna gwarantować
//poprawność hocona, np gdy wystepuje overloading constructora albo factory method to dokladnie adnotować typy.
object BeanFactory {

  private def clsByName(name: String): Class[_] = this.getClass.getClassLoader.loadClass(name)

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

  def createInstance[T](beanDef: BeanDef[T], context: Map[String, Any]): T = beanDef match {
    case Constructor(clsObj, args, setters) =>
      clsObj.getConstructors.filter { c =>
        parametersMatch(c.getParameterTypes, args)
      }.head.newInstance(args.map(a => createInstance(a.value, context))).asInstanceOf[T]
    case FactoryMethod(cls, clsName, methodName, args) =>
      ???
    case ListValue(_, values) =>
      ???
    case MapValue(_, _) => ???
    case PropertyValue(_, _) => ???
    case Referenced(_, _, value) =>
      ???
    case SimpleValue(_, value) =>
      ???
  }
}
