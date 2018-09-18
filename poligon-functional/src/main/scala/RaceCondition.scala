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

  def addressToBytes(sAddress: InetSocketAddress): Array[Byte] = {
    val result: Array[Byte] = new Array[Byte](sAddress.getAddress.getAddress.length + 1)
    val writer = new LimitedByteArrayOutputStream2(result, 0)
    val addressBytes = sAddress.getAddress.getAddress
    writer.write(addressBytes.length)
    writer.write(addressBytes)
    writer.data
  }

  def main(args: Array[String]): Unit = {
    var i = 0
    val expected = Array[Byte](4, 10, 10, 10, 10)
    val address = new InetSocketAddress("10.10.10.10", 1234)
    var count = 0
    var lastOk = true
    while (i < 1000000) {
      i += 1
      val data = addressToBytes(address)
      if (!data.sameElements(expected)) {
        count += 1
        if(lastOk){
          println(s"\n\nBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD $i \n\n")
        }
        lastOk = false
      } else {
        if(!lastOk){
          println(s"\n\nGOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOD $i\n\n")
        }
        lastOk = true
      }
    }
    println(s"Failures: $count")
  }
}