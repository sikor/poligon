import com.avsystem.commons.mongo.async.MongoObservableExtensions
import monix.reactive.Observable

import scala.util.matching.Regex

package object realestate extends MongoObservableExtensions {

  implicit class ObservableExt[T](val o: Observable[T]) extends AnyVal {
    def run(aggregate: Boolean = false, printer: T => String = x => x.toString): Unit = {
      NewEstate.run(o, aggregate, printer)
    }
  }

  implicit class RegexExt(val regex: Regex) extends AnyVal {
    def getGroup(str: String, index: Int = 1): Option[String] = regex.findAllMatchIn(str).toVector.headOption.map { m =>
      m.group(1)
    }

    def requireGroup(str: String, index: Int = 1): String = getGroup(str, index).getOrElse(throw new IllegalArgumentException(s"Failed to find $regex in $str"))
  }

}
