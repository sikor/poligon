package realestate

import com.avsystem.commons.misc.Timestamp
import com.avsystem.commons.mongo.mongoId
import com.avsystem.commons.mongo.scala.GenCodecCollection
import com.avsystem.commons.serialization.HasGenCodec
import monix.execution.Scheduler
import monix.reactive.Observable
import org.mongodb.scala.{Completed, MongoClient}
import scalaj.http.Http

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object NewEstate {

  private implicit val Scheduler: Scheduler = monix.execution.Scheduler.Implicits.global

  case class Investment()

  object Investment extends HasGenCodec[Investment]

  case class Dominium(@mongoId id: String, updateTime: Timestamp, investmentsUrls: Vector[String])

  object Dominium extends HasGenCodec[Dominium]

  case class Config(googleMapApiKey: String, @mongoId id: String = "latest")

  object Config extends HasGenCodec[Config]

  val UrlPrefix = "https://maps.googleapis.com/maps/api/distancematrix/json"

  val TestMapsApiUrl = "https://maps.googleapis.com/maps/api/distancematrix/json?origins=Boston,MA|Charlestown,MA&destinations=Lexington,MA|Concord,MA&departure_time=now&key="

  private val regex = """<a href="https://www.dominium.pl/pl/inwestycje/szczegoly/opis/([^"]+)""".r

  private val mongoClient = MongoClient()
  private val realEstateDB = mongoClient.getDatabase("realestate")
  private val dominiumCollection = GenCodecCollection.create[Dominium](realEstateDB, "Dominium")
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

  def updateDominiumInvestmentsList(): Observable[Completed] = {
    dominiumCollection.insertOne(Dominium("latest", Timestamp(System.currentTimeMillis()), getAllPages())).asMonix
  }

  def dominiumInvestmentsList(): Observable[Vector[String]] = {
    dominiumCollection.find().asMonix.map(_.investmentsUrls)
  }

  def main(args: Array[String]): Unit = run {
    dominiumInvestmentsList().map(_.size)
  }

  def run[A](program: Observable[A]): Unit = {
    Await.result(program.foreach(println), Duration.Inf)
  }


}
