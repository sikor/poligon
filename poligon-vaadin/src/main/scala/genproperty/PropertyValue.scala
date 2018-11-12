package genproperty

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.serialization.GenRef.Creator
import com.avsystem.commons.serialization.RawRef.Field
import com.avsystem.commons.serialization._
import genproperty.PropertyValue._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try


sealed abstract class PropertyValue[R](private val parent: Opt[AnyComplexProperty]) {
  self =>

  type RawType

  protected def setValue(v: RawType): Unit

  protected def getValue: RawType

  protected def childValueChanged(): Unit = {
    listeners.foreach(l => Try(l.valueChanged()))
  }

  private lazy val listeners: ArrayBuffer[ValueListener] = new ArrayBuffer[ValueListener]()

  def listen(onValueChange: R => Unit)(implicit codec: GenCodec[R]): Unit = {
    listeners += new ValueListener {
      override def valueChanged(): Unit = onValueChange(codec.read(new PropertyInput(self)))

      override def valueRemoved(): Unit = {}
    }
  }

  def get(implicit codec: GenCodec[R]): R = GenCodec.read(new PropertyInput(self))

  def set(newValue: R)(implicit codec: GenCodec[R]): Unit = {
    val output = new PropertyOutput(v => setValue(v.asInstanceOf[RawType]), Opt.Empty)
    codec.write(output, newValue)
    parent.foreach(_.childValueChanged())
  }
}

object PropertyValue {

  type GenProperty[R] = PropertyValue[R]
  type AnyProperty = PropertyValue[_]
  type AnyComplexProperty = ComplexProperty[_]
  type AnyObjectProperty = ObjectProperty[_]
  type AnyListProperty = ListProperty[_, _]
  private type ObjectType = mutable.LinkedHashMap[String, AnyProperty]
  private type ListType = ArrayBuffer[AnyProperty]

  private trait ValueListener {
    def valueChanged(): Unit

    def valueRemoved(): Unit
  }

  abstract class ComplexProperty[R](parent: Opt[AnyComplexProperty]) extends PropertyValue[R](parent)

  class ObjectProperty[R](private var value: ObjectType, parent: Opt[AnyComplexProperty]) extends ComplexProperty[R](parent) {

    type RawType = ObjectType

    def subProperty[S](creator: Creator[R] => GenRef[R, S])(implicit genRefCreator: GenRef.Creator[R]): GenProperty[S] = {
      val genRef = creator(implicitly[Creator[R]])
      val ref = genRef.rawRef.normalize.toList
      getSubValue(ref).asInstanceOf[GenProperty[S]]
    }

    private def getSubValue(ref: String): AnyProperty = value.asInstanceOf[ObjectType](ref)

    private def getSubValue(ref: List[SimpleRawRef]): AnyProperty =
      ref.foldLeft[AnyProperty](this)((ac, r) => ac.asInstanceOf[AnyObjectProperty].getSubValue(r.asInstanceOf[Field].name))

    protected def setValue(v: ObjectType): Unit = value = v

    protected[genproperty] def getValue: ObjectType = value
  }

  class ListProperty[L[_] <: Seq[_], I](private var value: ListType, parent: Opt[AnyComplexProperty]) extends ComplexProperty[L[I]](parent) {
    type RawType = ListType

    protected def setValue(v: ListType): Unit = value = v

    protected[genproperty] def getValue: ListType = value
  }

  class SimpleProperty[R](var value: Any, parent: Opt[AnyComplexProperty]) extends PropertyValue[R](parent) {
    type RawType = Any

    protected def setValue(v: Any): Unit = value = v

    protected def getValue: Any = value
  }

  class PropertyInput(value: AnyProperty) extends Input {

    override def readNull(): Boolean = value.getValue == null

    private def readPrimitive[T]: T = value.getValue.asInstanceOf[T]

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
      private val it = value.asInstanceOf[AnyListProperty].getValue.iterator

      override def nextElement(): Input = new PropertyInput(it.next())

      override def hasNext: Boolean = it.hasNext
    }

    override def readObject(): ObjectInput = new ObjectInput {
      private val fields = value.asInstanceOf[AnyObjectProperty].getValue.iterator

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

  class PropertyOutput(consumer: AnyProperty => Unit, parent: Opt[AnyComplexProperty]) extends Output {

    private def writePrimitive(value: Any): Unit = {
      consumer(new SimpleProperty(value, parent))
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

      private val listProp = new ListProperty(new ArrayBuffer[AnyProperty](), parent)

      override def writeElement(): Output = new PropertyOutput(v => listProp.getValue += v, Opt(listProp))

      override def finish(): Unit = consumer(listProp)
    }

    override def writeObject(): ObjectOutput = new ObjectOutput {
      private val objectProp = new ObjectProperty(new mutable.LinkedHashMap[String, AnyProperty], parent)

      override def writeField(key: String): Output = new PropertyOutput(v => objectProp.getValue += (key -> v), Opt(objectProp))

      override def finish(): Unit = consumer(objectProp)
    }
  }

}
