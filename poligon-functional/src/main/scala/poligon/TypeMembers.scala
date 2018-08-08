package poligon

object TypeMembers {

  trait BaseFields {
    type ValueType
  }

  sealed trait Fields extends BaseFields {

    def consume(v: ValueType): Unit = {}

    def create: ValueType
  }

  case object Field1 extends Fields {
    type ValueType = String

    def create: ValueType = "aaabbb"
  }

  case object Field2 extends Fields {
    type ValueType = Int

    def create: ValueType = 1234
  }

  case class Wrapper[T](value: T)

  case class Container[T <: Fields](field: T, value: Wrapper[_])

  trait TContainer {
    self =>
    type ValueType
    val field: Fields {type ValueType = self.ValueType}

    val value: Wrapper[self.ValueType]
  }

  def create[T](field0: Fields {type ValueType = T}, value0: Wrapper[T]): TContainer {type ValueType = T} = {
    new TContainer {
      type ValueType = T
      val field: Fields {
        type ValueType = T
      } = field0
      val value: Wrapper[T] = value0
    }
  }

  val tInst: TContainer {
    type ValueType = String
  } = create(Field1, Wrapper("123"))

  val tInst2: TContainer {
    type ValueType = Int
  } = create(Field2, Wrapper(123))

  val someField: Fields = Field1

  val tInst3 = create(someField, Wrapper(someField.create))

  val coll: Vector[TContainer] = Vector(tInst, tInst2, tInst3)

  case class SecondKey[T](name: String)

  type Generified[T] = Fields {type ValueType = T}

  val stringKey: SecondKey[String] = SecondKey[String]("aaa")
  val intKey: SecondKey[Int] = SecondKey[Int]("bbb")

  def getSecondKey[T](field: Generified[T]): SecondKey[T] = field match {
    case Field1 => stringKey
    case Field2 => intKey
  }


  def analyse[T](field: Generified[T], value: Wrapper[T], secondKey: SecondKey[T]): T = {
    field.consume(value.value)
    field.create

    field match {
      case Field1 => "aaa"
      case Field2 => 123
    }
  }


  def main(args: Array[String]): Unit = {
    coll.foreach { c =>
      val f = c.field
      val v = c.value.value
      f.consume(v)
      println(analyse[c.ValueType](f, c.value, getSecondKey(f)))
    }
  }
}
