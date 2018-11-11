package genproperty

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.serialization.GenRef.Creator
import com.avsystem.commons.serialization._
import genproperty.PropertyValue._

object GenProperty {
  def create[T: GenCodec : GenRef.Creator](init: T): GenProperty[T] = {
    var result: PropertyValue = null
    val output = new PropertyOutput(result = _)
    GenCodec.write(output, init)
    new ObjectProperty[T](result.asObjectValue, Opt(init), Opt.Empty)
  }
}

sealed trait GenProperty[T] {
  val parent: Opt[GenProperty[_]]
  protected var lastValueCache: Opt[T]

  protected final def childValueChanged(): Unit = {
    lastValueCache = Opt.Empty
    parent.foreach(_.childValueChanged())
  }
}

class ObjectProperty[T: GenCodec : GenRef.Creator] private[genproperty](
                                                                         private val property: ObjectValue,
                                                                         protected var lastValueCache: Opt[T],
                                                                         val parent: Opt[GenProperty[_]]) extends GenProperty[T] {

  def get: T = lastValueCache.getOrElse {
    lastValueCache = Opt(GenCodec.read(new PropertyInput(property)))
    lastValueCache.get
  }

  def subObject[S: GenCodec : GenRef.Creator](creator: Creator[T] => GenRef[T, S]): ObjectProperty[S] = {
    val genRef = creator(implicitly[Creator[T]])
    val ref = genRef.rawRef.normalize.toList
    ref match {
      case Nil => this.asInstanceOf[ObjectProperty[S]]
      case other =>
        val child = property.getSubValue(other)
        new ObjectProperty[S](child.asInstanceOf[ObjectValue], Opt.Empty, Opt(this))
    }
  }
}

class SimpleProperty[T: GenCodec] private[genproperty](
                                                        private val property: PropertyValue,
                                                        protected var lastValueCache: Opt[T],
                                                        val parent: Opt[GenProperty[_]]
                                                      ) extends GenProperty[T] {

}

class ListProperty[I: GenCodec] private[genproperty](
                                                      private val property: ListValue,
                                                      protected var lastValueCache: Opt[Seq[I]],
                                                      val parent: Opt[GenProperty[_]]
                                                    ) extends GenProperty[Seq[I]] {

}