
class PatternMatch {

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


    def getValue8[X](f: Field {type T = X}): X = (f.ug: TField[X]) match {
      case Field1 => "abc"
      case Field2 => 123
    }

  //No exhaustive check
  def getValue[X](f: Field {type T = X}): X = f match {
    case Field1 => "aaa"
    //    case Field2 => 123
  }

  //casting needed
  def getValue2[X](f: Field {type T = X}): X = (f: Field) match {
    case Field1 => "aaa".asInstanceOf[X]
    case Field2 => 123.asInstanceOf[X]
  }

  type Generified[X] = Field {type T = X}

  //No exhaustive check
  def getValue3[X](f: Generified[X]): X = f match {
    case Field1 => "aaa"
    //    case Field2 => 123
  }

  sealed trait PField[T]

  case object PField1 extends PField[String]

  case object PField2 extends PField[Int]

  //all is fine but type parameter is needed
  def getValue4[X](f: PField[X]): X = f match {
    case PField1 => "aaa"
    case PField2 => 123
  }

}
