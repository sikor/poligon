import java.io.OutputStream
import java.net.InetSocketAddress

object RaceCondition {

  class LimitedByteArrayOutputStream2(val data: Array[Byte], var currentIndex: Int) extends OutputStream {
    override def write(bytes: Array[Byte], off: Int, len: Int): Unit = {
      System.arraycopy(bytes, off, data, currentIndex, len)
      currentIndex += bytes.length
    }

    def write(b: Int): Unit = {
      data.update(currentIndex, b.toByte)
      currentIndex += 1
    }
  }

  def addressToBytes(address: InetSocketAddress): Array[Byte] = {
    val sAddress = address
    val result: Array[Byte] = new Array[Byte](sAddress.getAddress.getAddress.length + 1)
    val writer = new LimitedByteArrayOutputStream2(result, 0)
    val addressBytes = sAddress.getAddress.getAddress
    //    result.update(0, addressBytes.length.toByte)
    //    System.arraycopy(addressBytes, 0, result, 1, addressBytes.length)
    writer.write(addressBytes.length)
    writer.write(addressBytes)
    writer.data
  }

  def main(args: Array[String]): Unit = {
    var i = 0
    val expected = Array[Byte](4, 127, 0, 0, 1)
    val address = new InetSocketAddress("localhost", 1234)
    var count = 0
    while (i < 10000000) {
      i += 1
      val data = addressToBytes(address)
      if (!data.sameElements(expected)) {
        count += 1
        //        println(i + data.mkString(" ", ", ", ""))
        //        throw new IllegalArgumentException(data.mkString(", "))
      }
    }
    println(s"Failures: $count")
  }
}