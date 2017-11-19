package poligon.parser.examples


sealed trait ProcessingType {
  def get: this.type = this
}

case object FastProcessing extends ProcessingType

case object PreciseProcessing extends ProcessingType

case class Strategy(processingTpe: ProcessingType)

class ImportantService(id: Int, name: String, customerName: String, strategy: Strategy)
