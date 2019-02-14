package poligon.exampleapp.services

import java.util.Date

import monix.reactive.Observable

import scala.concurrent.duration._

class CurrentTimeService {

  def currentTime: Observable[Date] = Observable.interval(100.millis).map(_ => new Date)
}
