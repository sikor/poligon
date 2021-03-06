package realestate

import java.io.{File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.time.ZoneId
import java.util.{Calendar, Locale, TimeZone}

import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion, Timestamp}
import com.avsystem.commons.mongo._
import com.avsystem.commons.mongo.scala.GenCodecCollection
import com.avsystem.commons.serialization.json.JsonStringInput
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec, HasGenCodec}
import com.mongodb.client.model.Filters
import com.typesafe.scalalogging.StrictLogging
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.mongodb.scala.result.UpdateResult
import org.mongodb.scala.{Completed, MongoClient}
import realestate.NewEstate.TravelMode.{Bicycling, Driving}
import scalaj.http.Http

import _root_.scala.annotation.tailrec
import _root_.scala.concurrent.Await
import _root_.scala.concurrent.duration.Duration
import _root_.scala.util.control.NonFatal

object NewEstate extends StrictLogging {

  private implicit val Scheduler: Scheduler = monix.execution.Scheduler.Implicits.global

  case class Coordinates(x: Double, y: Double) {
    def toLatLon: String = s"$y,$x"
  }

  object Coordinates extends HasGenCodec[Coordinates]

  case class ImportantLocation(name: String, xLoc: Double, yLoc: Double) {
    val coordinates = Coordinates(xLoc, yLoc)
  }

  case class Investment(@mongoId url: String, xLoc: Double, yLoc: Double, meterPrice: Option[Int], street: Option[String], district: String) {
    val coordinates = Coordinates(xLoc, yLoc)
  }

  object Investment extends HasGenCodec[Investment]

  case class Dominium(@mongoId id: String, updateTime: Timestamp, investmentsUrls: Vector[String])

  object Dominium extends HasGenCodec[Dominium]

  case class Config(googleMapApiKey: String, @mongoId id: String = "latest")

  object Config extends HasGenCodec[Config]

  sealed abstract class TravelMode(val name: String) extends NamedEnum

  object TravelMode extends NamedEnumCompanion[TravelMode] {

    case object Bicycling extends TravelMode("bicycling")

    case object Driving extends TravelMode("driving")


    implicit val Codec: GenCodec[TravelMode] = GenCodec.materialize[TravelMode]

    implicit val KeyCoec: GenKeyCodec[TravelMode] = GenKeyCodec.create(byName(_), _.name)
    override val values: List[TravelMode] = caseObjects
  }

  case class TimeAndDistance(timeInSec: Int, distanceInMeter: Int)

  object TimeAndDistance extends HasGenCodec[TimeAndDistance]

  case class Distance(origin: Coordinates, destination: Coordinates, distances: Map[TravelMode, TimeAndDistance])

  object Distance extends HasGenCodec[Distance]

  case class GDuration(value: Int, text: String)

  case class GDistance(value: Int, text: String)

  case class GElement(status: String, duration: GDuration, distance: GDistance, duration_in_traffic: Option[GDuration] = None)

  case class Column(elements: Vector[GElement])

  case class DistanceMatrix(status: String, origin_addresses: Vector[String], destination_addresses: Vector[String],
                            rows: Vector[Column])

  object DistanceMatrix {
    implicit val codec: GenCodec[DistanceMatrix] = GenCodec.materializeRecursively[DistanceMatrix]
  }

  val DistanceMatrixUrlPrefix = "https://maps.googleapis.com/maps/api/distancematrix/json"

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
  private val distanceCollection = GenCodecCollection.create[Distance](realEstateDB, "Distance")

  private val AV = ImportantLocation("AV", 19.899028, 50.0834354)
  private val GE = ImportantLocation("GE", 19.9926023, 50.0803362)
  private val MARKET = ImportantLocation("Market", 19.9345619, 50.0619005)

  private val DepartureTime = {
    val calendar = Calendar.getInstance(TimeZone.getTimeZone(ZoneId.of("Europe/Warsaw")))
    calendar.set(2018, 10, 9, 8, 0, 0)
    calendar.getTime
  }

  def setConfig(config: Config): Observable[Completed] = {
    configCollection.insertOne(config).asMonix
  }

  def config(): Observable[Config] = {
    configCollection.find().asMonix
  }


  //https://maps.googleapis.com/maps/api/distancematrix/json?units=imperial&origins=40.6655101,-73.89188969999998&destinations=40.6905615%2C-73.9976592%7C40.6905615%2C-73.9976592%7C40.6905615%2C-73.9976592%7C40.6905615%2C-73.9976592%7C40.6905615%2C-73.9976592%7C40.6905615%2C-73.9976592%7C40.659569%2C-73.933783%7C40.729029%2C-73.851524%7C40.6860072%2C-73.6334271%7C40.598566%2C-73.7527626%7C40.659569%2C-73.933783%7C40.729029%2C-73.851524%7C40.6860072%2C-73.6334271%7C40.598566%2C-73.7527626&key=YOUR_API_KEY

  //lat - y
  //lon - x
  def getDistanceWithCache(origin: Coordinates, dests: Vector[Coordinates]): Task[Vector[Distance]] = for {
    config <- config().headL
    coorWithOptDist <- getDistsFromCache(origin, dests)
    toFind = coorWithOptDist.collect {
      case (coor, None) => coor
    }
    _ <- {
      if (toFind.nonEmpty) {
        val gDistBike = getDistances(origin, toFind, Bicycling, config.googleMapApiKey)
        val gDistCar = getDistances(origin, toFind, Driving, config.googleMapApiKey)
        val distance = toFind.zipWithIndex.map { case (d, i) =>
          Distance(origin, d, Map(Bicycling -> gDistBike(i), Driving -> gDistCar(i)))
        }
        distanceCollection.insertMany(distance).asMonix.completedL
      } else {
        Task.unit
      }
    }
    coorWithOptDist2 <- getDistsFromCache(origin, dests)
    all = coorWithOptDist2.map {
      case (coor, Some(dist)) => dist
      case _ => throw new Exception
    }
  } yield all

  private def getDistsFromCache(origin: Coordinates, dests: Vector[Coordinates]): Task[Vector[(Coordinates, Option[Distance])]] = {
    Task.sequence(dests.map(dest =>
      distanceCollection.find(Filters.and(
        Filters.eq("origin.x", origin.x),
        Filters.eq("origin.y", origin.y),
        Filters.eq("destination.x", dest.x),
        Filters.eq("destination.y", dest.y))).asMonix.headOptionL.map(i => (dest, i))
    ))
  }

  val ImportantLocations = Vector(AV, GE, MARKET)

  def getAllDistances(investments: Observable[Investment] = investmentsDetails()): Observable[Distance] = {
    val destinations = ImportantLocations
    for {
      i <- investments
      dists <- Observable.fromTask(getDistanceWithCache(i.coordinates, destinations.map(_.coordinates)))
      dist <- Observable(dists: _*)
    } yield dist
  }

  def getDistances(origin: Coordinates, destinations: Vector[Coordinates], mode: TravelMode, apiKey: String): Vector[TimeAndDistance] = {
    val params = Seq(
      ("origins", origin.toLatLon),
      ("destinations", destinations.map(_.toLatLon).mkString("|")),
      ("key", apiKey),
      ("mode", mode.name),
      ("traffic_model", "pessimistic"),
      ("departure_time", DepartureTime.toInstant.getEpochSecond.toString))
    logger.info(s"params: $params")
    val res = Http(DistanceMatrixUrlPrefix).copy(params = params).asString.body
    logger.info(res)
    val matrix = JsonStringInput.read[DistanceMatrix](res)
    val row = matrix.rows.head
    row.elements.map { e =>
      TimeAndDistance(e.duration_in_traffic.getOrElse(e.duration).value, e.distance.value)
    }
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

  def investmentsWithDistances(): Observable[(Investment, Vector[(ImportantLocation, Distance)])] = {
    investmentsDetails().flatMap { i =>
      val locationWithDistance = getDistanceWithCache(i.coordinates, ImportantLocations.map(_.coordinates))
        .map(dist => ImportantLocations.zip(dist))
      Observable.fromTask(locationWithDistance).map(v => (i, v))
    }
  }

  def investmentWithDistancesPrinter(i: (Investment, Vector[(ImportantLocation, Distance)])): String = {
    i match {
      case (investment, dists) =>
        s"${investment.street.getOrElse(investment.district)} (${investment.meterPrice.getOrElse("?")} zł/m): " +
          s"${
            dists.map { case (loc, dist) => s"${loc.name}: ${dist.distances(Bicycling).distanceInMeter.toDouble / 1000} km, " +
              s"${dist.distances(Driving).timeInSec.toDouble / 60} min"
            }.mkString(", ")
          }"
    }
  }

  def format(d: Double): String = {
    String.format(Locale.FRANCE, "%.2f", d.asInstanceOf[AnyRef])
  }


  def csvInvestmentWithDistancesPrinter(i: (Investment, Vector[(ImportantLocation, Distance)])): String = i match {
    case (investment, distances) =>
      (Vector(investment.url, investment.street.getOrElse(""), investment.district, investment.meterPrice.getOrElse(0).toString) ++
        distances.flatMap {
          case (loc, distance) => TravelMode.values.flatMap { m =>
            val mDistance = distance.distances(m)
            Vector(mDistance.timeInSec / 60.0, mDistance.distanceInMeter / 1000.0).map(d => format(d))
          }
        }).map(s => s.replaceAllLiterally(";", ",")).mkString("; ")
  }

  def investmentWithDistancesTitleRow(locations: Vector[ImportantLocation]): String =
    (Vector("url", "street", "district", "meter price") ++ (for {
      loc <- locations
      m <- TravelMode.values
      t <- Vector("time [minutes]", "distance [km]")
    } yield s"${loc.name} (${m.name}) - $t")).mkString("; ")

  def generateCsvRaport(filePath: String = "/tmp/investmentsReport.csv"): Unit = {
    val rest = investmentsWithDistances().run(aggregate = true)
    val file = new File(filePath)
    file.delete()
    file.createNewFile()
    val writer = new OutputStreamWriter(new FileOutputStream(file), StandardCharsets.UTF_8)

    def writeln(l: String): Unit = writer.write(l + "\n")

    writeln(investmentWithDistancesTitleRow(ImportantLocations))
    rest.foreach(d => writeln(csvInvestmentWithDistancesPrinter(d)))
    writer.close()
  }

  def main(args: Array[String]): Unit = generateCsvRaport()

  def run[A](program: Observable[A], aggregate: Boolean = false, printer: A => String = (x: A) => x.toString): Vector[A] = {
    val result = Vector.newBuilder[A]
    Await.result(program.foreach { m =>
      println(printer(m))
      if (aggregate) {
        result += m
      }
    }, Duration.Inf)
    result.result()
  }


}
