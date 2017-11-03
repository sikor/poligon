package poligon

import java.io.InputStream
import java.nio.ByteBuffer


object StreamingDecoders {

  trait LeafState

  case class Bufferring(buffer: ByteBuffer) extends LeafState

  case class Child(partial: Partial[_]) extends LeafState

  trait DecodingState[T]

  case class Decoded[T](value: T) extends DecodingState[T]

  case class Partial[T](readyChildren: Vector[Decoded[_]], leafState: LeafState)

  trait StreamingDecoder[T] {
    def decode(input: InputStream, currentState: Partial[T]): DecodingState[T]
  }

}
