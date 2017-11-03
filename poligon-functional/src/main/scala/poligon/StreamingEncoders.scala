package poligon

import java.io.{ByteArrayOutputStream, OutputStream}
import java.nio.ByteBuffer

import scala.annotation.tailrec


case class ComplicatedObj(id: Int, data: Array[Byte], data2: Array[Byte])


object StreamingEncoders {

  trait StreamingEncoder[T] {
    self =>
    def get(value: T, offset: Int, len: Int, target: OutputStream): Unit

    def length(value: T): Int

    def toArray(value: T, offset: Int, len: Int): Array[Byte] = {
      val out = new ByteArrayOutputStream()
      get(value, offset, len, out)
      out.toByteArray
    }

    def map[E](mapper: E => T): StreamingEncoder[E] =
      new StreamingEncoder[E] {
        def get(value: E, offset: Int, len: Int, target: OutputStream): Unit =
          self.get(mapper(value), offset, len, target)

        def length(value: E): Int = self.length(mapper(value))
      }

  }

  object ArrayStreamingEncoder extends StreamingEncoder[Array[Byte]] {
    def get(value: Array[Byte], offset: Int, len: Int, target: OutputStream): Unit =
      target.write(value, offset, len)

    def length(value: Array[Byte]): Int = value.length
  }

  object IntStreamingEncoder extends StreamingEncoder[Int] {
    override def get(value: Int, offset: Int, len: Int, target: OutputStream): Unit = {
      require(offset >= 0 && offset <= 4, s"offset: $offset, len: $len")
      require(offset + len <= 4, s"offset: $offset, len: $len")
      val buffer = ByteBuffer.allocate(4)
      buffer.putInt(value)
      ArrayStreamingEncoder.get(buffer.array(), offset, len, target)
    }

    override def length(value: Int): Int = 4
  }

  class AggregatedEncoder[T](encoders: Vector[StreamingEncoder[T]]) extends StreamingEncoder[T] {
    override def get(value: T, offset: Int, len: Int, target: OutputStream): Unit = {
      require(offset >= 0 && len >= 0, s" offset: $offset, len: $len, value: $value")
      require(offset + len <= length(value), s"${offset + len} > ${length(value)}, offset: $offset, len: $len, value: $value")
      val targetEnd = offset + len

      @tailrec
      def iterate(currentStart: Int, currentEncoderIndex: Int): Unit = {
        val currentLength = encoders(currentEncoderIndex).length(value)
        val interestingOffset = Math.max(0, offset - currentStart)
        val interestingLen = targetEnd - currentStart - interestingOffset
        val availableLength = currentLength - interestingOffset
        if (interestingLen > 0 && availableLength > 0) {
          encoders(currentEncoderIndex).get(value, interestingOffset, Math.min(availableLength, interestingLen), target)
        }
        if (interestingOffset >= currentLength || availableLength < interestingLen) {
          iterate(currentStart + currentLength, currentEncoderIndex + 1)
        }
      }

      iterate(0, 0)
    }

    override def length(value: T): Int = encoders.map(_.length(value)).sum
  }

  def encodePart[T, E](mapper: T => E)(encoder: StreamingEncoder[E]): StreamingEncoder[T] =
    new StreamingEncoder[T] {
      def get(value: T, offset: Int, len: Int, target: OutputStream): Unit = {
        require(offset >= 0 && len >= 0, s"offset: $offset, len: $len")
        require(offset + len <= length(value), s"${offset + len} > ${length(value)}, offset: $offset, len: $len")
        encoder.get(mapper(value), offset, len, target)
      }

      def length(value: T): Int = encoder.length(mapper(value))
    }

  val ComplicatedObjEncoder = new AggregatedEncoder(Vector[StreamingEncoder[ComplicatedObj]](
    IntStreamingEncoder.map(_.id),
    ArrayStreamingEncoder.map(_.data),
    ArrayStreamingEncoder.map(_.data2)
  ))


}