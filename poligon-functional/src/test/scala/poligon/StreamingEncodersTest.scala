package poligon

import org.scalatest.FunSuite

class StreamingEncodersTest extends FunSuite {

  test("Test complicated object encoding") {
    val obj = ComplicatedObj(1, Array[Byte](2, 3, 4, 5), Array[Byte](6, 7, 8, 9, 10))
    val fullResult = Array[Byte](0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    assert(StreamingEncoders.ComplicatedObjEncoder.length(obj) == fullResult.length)

    def pairs = for {
      offset <- Range(0, fullResult.length - 1)
      len <- Range(1, fullResult.length - offset)
    } yield (offset, len)

    pairs.foreach {
      case (offset, len) =>
        val expected = fullResult.slice(offset, offset + len)
        val target = StreamingEncoders.ComplicatedObjEncoder.toArray(obj, offset, len)
        assert(expected sameElements target, s"offset: $offset, len: $len, expected: ${expected.toVector}, got: ${target.toVector}")
    }
  }

}
