import com.avsystem.commons.mongo.async.MongoObservableExtensions
import monix.reactive.Observable

package object realestate extends MongoObservableExtensions {

  implicit class ObservableExt[T](val o: Observable[T]) extends AnyVal {
    def run(): Unit = {
      NewEstate.run(o)
    }
  }

}
