package poligon

object CrossPatternMatch {

  sealed trait WholeFamily

  trait SubFamily {
    def fromSub: String = "a"
  }

  case object Instance1 extends WholeFamily

  case object Instance2 extends WholeFamily

  case object Instance3 extends WholeFamily

  case object SubFamily1 extends WholeFamily with SubFamily

  case object SubFamily2 extends WholeFamily with SubFamily

  sealed trait Wrappers[+T]

  case class Wrapper[+T](implicit val instance: T with SubFamily) extends Wrappers[T]

  val w: Wrapper[SubFamily1.type] = Wrapper()(SubFamily1)


  def patternMatch(wrapper: Wrappers[WholeFamily]): SubFamily = {
    wrapper match {
      //      case Wrapper(instance) => instance
      case w: Wrapper[WholeFamily] => w.instance match {
        case SubFamily1 => SubFamily1
        case SubFamily2 => SubFamily2
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val w = Wrapper()(SubFamily2)
    patternMatch(w)
  }

}
