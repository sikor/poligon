package genproperty

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.serialization.{GenCodec, GenRef}
import com.github.ghik.silencer.silent

@silent
class CastableProperty[T](private val value: PropertyValue, val parent: GenProperty[_]) {
  def asObjectProperty(implicit codec: GenCodec[T], genRefCreator: GenRef.Creator[T]): ObjectProperty[T] = {
    new ObjectProperty[T](value.asObjectValue, Opt.Empty, Opt(parent))
  }

  def asListProperty[I: GenCodec](implicit seqEvidence: T =:= Seq[I]): ListProperty[I] = {
    new ListProperty[I](value.asListValue, Opt.Empty, Opt(parent))
  }
}
