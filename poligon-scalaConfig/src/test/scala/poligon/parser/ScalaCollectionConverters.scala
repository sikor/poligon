package poligon.parser

import com.avsystem.commons.jiop.JavaInterop._
import org.springframework.core.convert.converter.Converter

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.{immutable, mutable}

/**
  * Author: ghik
  * Created: 24/07/15.
  */
object ScalaCollectionConverters {

  abstract class JListToScalaCollectionConverter[T, To[_]](implicit cbf: CanBuildFrom[Nothing, T, To[T]])
    extends Converter[JList[T], To[T]] {

    def convert(source: JList[T]): To[T] = source.asScala.to[To]
  }

  abstract class JSetToScalaCollectionConverter[T, To[_]](implicit cbf: CanBuildFrom[Nothing, T, To[T]])
    extends Converter[JSet[T], To[T]] {

    def convert(source: JSet[T]): To[T] = source.asScala.to[To]
  }

  class JListToSeqConverter[T] extends JListToScalaCollectionConverter[T, Seq]

  class JListToImmutableSeqConverter[T] extends JListToScalaCollectionConverter[T, immutable.Seq]

  class JListToVectorConverter[T] extends JListToScalaCollectionConverter[T, Vector]

  class JListToListConverter[T] extends JListToScalaCollectionConverter[T, List]

  class JListToSetConverter[T] extends JListToScalaCollectionConverter[T, Set]

  class JSetToSetConverter[T] extends JSetToScalaCollectionConverter[T, Set]

  class JListToHashSetConverter[T] extends JListToScalaCollectionConverter[T, HashSet]

  class JSetToHashSetConverter[T] extends JSetToScalaCollectionConverter[T, HashSet]

  class JListToMutableLinkedHashSetConverter[T] extends JListToScalaCollectionConverter[T, mutable.LinkedHashSet]

  abstract class ScalaMapConverter[K, V, To[_, _]](implicit cbf: CanBuildFrom[Nothing, (K, V), To[K, V]])
    extends Converter[JMap[K, V], To[K, V]] {

    def convert(source: JMap[K, V]): To[K, V] = (cbf() ++= source.asScala).result()
  }

  class ToMapConverter[K, V] extends ScalaMapConverter[K, V, Map]

  class ToHashMapConverter[K, V] extends ScalaMapConverter[K, V, HashMap]

}

