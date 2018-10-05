package realestate

import com.avsystem.commons.misc.Timestamp
import com.avsystem.commons.mongo._
import com.avsystem.commons.mongo.scala.GenCodecCollection
import com.avsystem.commons.serialization.HasGenCodec
import com.mongodb.client.model.Filters
import com.typesafe.scalalogging.StrictLogging
import monix.execution.Scheduler
import monix.reactive.Observable
import org.mongodb.scala.result.UpdateResult
import org.mongodb.scala.{Completed, MongoClient}
import scalaj.http.Http

import _root_.scala.annotation.tailrec
import _root_.scala.concurrent.Await
import _root_.scala.concurrent.duration.Duration
import _root_.scala.util.control.NonFatal

object NewEstate extends StrictLogging {

  private implicit val Scheduler: Scheduler = monix.execution.Scheduler.Implicits.global

  case class Investment(@mongoId url: String, xLoc: Double, yLoc: Double, meterPrice: Option[Int], street: Option[String], district: String)

  object Investment extends HasGenCodec[Investment]

  case class Dominium(@mongoId id: String, updateTime: Timestamp, investmentsUrls: Vector[String])

  object Dominium extends HasGenCodec[Dominium]

  case class Config(googleMapApiKey: String, @mongoId id: String = "latest")

  object Config extends HasGenCodec[Config]


  val UrlPrefix = "https://maps.googleapis.com/maps/api/distancematrix/json"

  val TestMapsApiUrl = "https://maps.googleapis.com/maps/api/distancematrix/json?origins=Boston,MA|Charlestown,MA&destinations=Lexington,MA|Concord,MA&departure_time=now&key="

  private val regex = """<a href="https://www.dominium.pl/pl/inwestycje/szczegoly/opis/([^"]+)""".r
  private val PriceRangeRegex = """Cena brutto</p>[^>]+[>]([^-]+)-([^z]+)zł/m""".r
  private val PriceRegex = """Cena brutto</p>[^>]+[>]([^z]+)zł/m""".r
  private val DistrictRegex = """Dzielnica / miejscowość</p>[^>]+[>]([^<]+)</p>""".r
  private val StreetRegex = """Ulica / lokalizacja</p>[^>]+[>]([^<]+)</p>""".r
  private val XCoordRegex = """data-x="([\d.]+)"""".r
  private val YCoordRegex = """data-y="([\d.]+)"""".r
  private val OpisPrefix = "https://www.dominium.pl/pl/inwestycje/szczegoly/opis/"
  private val MapPrefix = "https://www.dominium.pl/pl/inwestycje/szczegoly/mapa/"

  private val mongoClient = MongoClient()
  private val realEstateDB = mongoClient.getDatabase("realestate")
  private val dominiumCollection = GenCodecCollection.create[Dominium](realEstateDB, "Dominium")
  private val invesmentCollection = GenCodecCollection.create[Investment](realEstateDB, "Investment")
  private val configCollection = GenCodecCollection.create[Config](realEstateDB, "Config")

  def setConfig(config: Config): Observable[Completed] = {
    configCollection.insertOne(config).asMonix
  }

  def config(): Observable[Config] = {
    configCollection.find().asMonix
  }

  def getInvestmentsPage(i: Int): Vector[String] = {
    val response = Http(s"https://www.dominium.pl/inwestycje/wszystkie/krakow/$i").asString
    if (response.code == 200) {
      regex.findAllMatchIn(response.body).map(_.group(1)).toVector
    } else {
      Vector.empty
    }
  }

  @tailrec
  def getAllPages(acc: Vector[String] = Vector.empty, i: Int = 0): Vector[String] = {
    val result = getInvestmentsPage(i)
    val newAcc = acc ++ result
    if (result.isEmpty) {
      newAcc
    } else {
      getAllPages(newAcc, i + 1)
    }
  }

  def getInvestmentDetails(url: String): Investment = {
    val fullUrl = s"$OpisPrefix$url"
    try {
      val details = Http(fullUrl).asString.body

      def getPriceRanges(details: String): Option[Int] = {
        PriceRangeRegex.findAllMatchIn(details).toVector.headOption.map { m =>
          val from = m.group(1).replace(" ", "")
          val to = m.group(2).replace(" ", "")
          (from.toInt + to.toInt) / 2
        }
      }

      //6 399 - 7 349 zł/m2
      val price = getPriceRanges(details).orElse(PriceRegex.getGroup(details).map(_.replace(" ", "").toInt))

      val map = Http(s"$MapPrefix$url").asString.body

      val district = DistrictRegex.requireGroup(map)
      val street = StreetRegex.getGroup(map)
      val xCoord = XCoordRegex.requireGroup(map).toDouble
      val yCoord = YCoordRegex.requireGroup(map).toDouble

      val res = Investment(url, xCoord, yCoord, price, street, district)
      res
    } catch {
      case NonFatal(e) => throw new Exception(s"Failed to describe: $fullUrl", e)
    }
  }

  def updateDominiumInvestmentsList(): Observable[UpdateResult] = {
    dominiumCollection.replaceOne(
      Filters.eq("_id", "latest"),
      Dominium("latest", Timestamp(System.currentTimeMillis()), getAllPages())).asMonix
  }

  def dominiumInvestmentsList(): Observable[Vector[String]] = {
    dominiumCollection.find().asMonix.map(_.investmentsUrls)
  }

  def investmentsDetails(): Observable[Investment] = {
    invesmentCollection.find().asMonix
  }

  def updateInvestmentsDetails(urls: Observable[Vector[String]] = dominiumInvestmentsList()): Observable[Completed] = for {
    all <- urls
    url <- Observable(all: _*)
    count <- invesmentCollection.countDocuments(Filters.eq("_id", url)).asMonix
    _ <- if (count == 0) {
      val details = getInvestmentDetails(url)
      logger.info(details.toString)
      invesmentCollection.insertOne(details).asMonix
    } else {
      logger.info(s"already have: $url")
      Observable.empty
    }
  } yield Completed()

  def main(args: Array[String]): Unit = investmentsDetails().run()

  def run[A](program: Observable[A]): Unit = {
    Await.result(program.foreach(println), Duration.Inf)
  }


}
