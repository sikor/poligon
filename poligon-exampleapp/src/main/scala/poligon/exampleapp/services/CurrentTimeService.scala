package poligon.exampleapp.services

import java.time.Instant

import monix.reactive.Observable

import scala.concurrent.duration._

class CurrentTimeService {

  def currentTime: Observable[Instant] = Observable.interval(100.millis).map(_ => Instant.now)
}
