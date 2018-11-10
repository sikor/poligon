package genproperty

import com.avsystem.commons.serialization.RawRef.Field
import com.avsystem.commons.serialization._
import genproperty.PropertyValue.{ListValue, ObjectValue, Primitive}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


sealed trait PropertyValue {
  def getSubValue(ref: String): PropertyValue = this match {
    case o: ObjectValue => o.value(ref)
    case _ => throw new IllegalArgumentException(s"Cannot take subvalue of $this")
  }

  def getSubValue(ref: List[SimpleRawRef]): PropertyValue =
    ref.foldLeft(this)((ac, r) => ac.getSubValue(r.asInstanceOf[Field].name))

  def asObjectValue: ObjectValue = {
    this match {
      case o: ObjectValue => o
      case _ => throw new IllegalArgumentException
    }
  }

  def asListValue: ListValue = {
    this match {
      case o: ListValue => o
      case _ => throw new IllegalArgumentException
    }
  }

  def asPrimitive: Primitive = {
    this match {
      case p: Primitive => p
      case _ => throw new IllegalArgumentException
    }
  }
}

object PropertyValue {

  class Primitive(var value: Any) extends PropertyValue

  class ObjectValue(val value: mutable.Map[String, PropertyValue]) extends PropertyValue

  class ListValue(val value: ArrayBuffer[PropertyValue]) extends PropertyValue

  class PropertyInput(value: PropertyValue) extends Input {

    override def readNull(): Boolean = value == null

    private def readPrimitive[T]: T = value.asInstanceOf[Primitive].value.asInstanceOf[T]

    override def readSimple(): SimpleInput = new SimpleInput {
      override def readString(): String = readPrimitive[String]

      override def readBoolean(): Boolean = readPrimitive[Boolean]

      override def readInt(): Int = readPrimitive[Int]

      override def readLong(): Long = readPrimitive[Long]

      override def readDouble(): Double = readPrimitive[Double]

      override def readBigInt(): BigInt = readPrimitive[BigInt]

      override def readBigDecimal(): BigDecimal = readPrimitive[BigDecimal]

      override def readBinary(): Array[Byte] = readPrimitive[Array[Byte]]
    }

    override def readList(): ListInput = new ListInput {
      private val it = value.asInstanceOf[ListValue].value.iterator

      override def nextElement(): Input = new PropertyInput(it.next())

      override def hasNext: Boolean = it.hasNext
    }

    override def readObject(): ObjectInput = new ObjectInput {
      private val fields = value.asInstanceOf[ObjectValue].value.iterator

      override def nextField(): FieldInput = {
        val field = fields.next()
        new PropertyInput(field._2) with FieldInput {
          override def fieldName: String = field._1
        }
      }

      override def hasNext: Boolean = fields.hasNext
    }

    override def skip(): Unit = ()
  }

  class PropertyOutput(consumer: PropertyValue => Unit) extends Output {

    private def writePrimitive(value: Any): Unit = {
      consumer(new Primitive(value))
    }

    override def writeNull(): Unit = writePrimitive(null)

    override def writeSimple(): SimpleOutput = new SimpleOutput {
      override def writeString(str: String): Unit = writePrimitive(str)

      override def writeBoolean(boolean: Boolean): Unit = writePrimitive(boolean)

      override def writeInt(int: Int): Unit = writePrimitive(int)

      override def writeLong(long: Long): Unit = writePrimitive(long)

      override def writeDouble(double: Double): Unit = writePrimitive(double)

      override def writeBigInt(bigInt: BigInt): Unit = writePrimitive(bigInt)

      override def writeBigDecimal(bigDecimal: BigDecimal): Unit = writePrimitive(bigDecimal)

      override def writeBinary(binary: Array[Byte]): Unit = writePrimitive(binary)
    }

    override def writeList(): ListOutput = new ListOutput {

      private val buffer = new ArrayBuffer[PropertyValue]()

      override def writeElement(): Output = new PropertyOutput(v => buffer += v)

      override def finish(): Unit = consumer(new ListValue(buffer))
    }

    override def writeObject(): ObjectOutput = new ObjectOutput {
      private val buffer = new mutable.LinkedHashMap[String, PropertyValue]

      override def writeField(key: String): Output = new PropertyOutput(v => buffer += (key -> v))

      override def finish(): Unit = consumer(new ObjectValue(buffer))
    }
  }

}
