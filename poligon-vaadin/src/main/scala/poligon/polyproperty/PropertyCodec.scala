package poligon.polyproperty

trait PropertyCodec[T] {
  def newProperty(value: T): Property[T]

  def updateProperty(value: T, property: Property[T]): Unit

  def readProperty(property: Property[T]): T
}

object PropertyCodec {

}