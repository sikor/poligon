object TraitOverriding {

  trait Base {
    def field: String = "Base"
  }

  abstract class Field(override val field: String) extends Base

  trait Over extends Base {
    override val field: String = "Over"
  }

  trait Over2 extends Base {
    override val field: String = "Over2"
  }

  case object Result extends Field("Field") with Over with Over2

  def main(args: Array[String]): Unit = {
    println(Result.field)
  }

}
