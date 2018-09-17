
object InferImplicitType {

  sealed trait Field {
    type T

    def ug: TField[T]
  }

  sealed trait TField[G] extends Field {
    final type T = G

    def ug: TField[T] = this
  }

  case object Field1 extends TField[String]

  case object Field2 extends TField[Int]

  trait TC[S, T] extends Simple[S]

  trait Simple[S]

  implicit object STC extends TC[String, String]

  implicit object ITC extends TC[Int, Int]

  class Getter[S] {
    def get[T](implicit value: TC[S, T]): TC[S, T] = value
  }


  def map[X](field: TField[X]): Simple[X] = field match {
    case Field1 => new Getter[String].get[String]
    case Field2 => new Getter[Int].get[Int]
  }

  def main(args: Array[String]): Unit = {
    println(map(Field1))
  }
}
