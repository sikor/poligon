package realestate

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.serialization.GenCodec
import com.avsystem.commons.serialization.json.{JsonReader, JsonStringInput}

import scala.io.Source

object Main {

  val example =
    """
      |{"id":"55553240",
      |"user_id":"413364",
      |"created_at":"2018-09-04 20:19:27",
      |"created_at_first":"2018-09-04 02:20:32",
      |"valid_to":"2018-10-04 02:20:35",
      |"status":"active",
      |"description":"1 pokojowe z klimatem ul. Gnie\u017anie\u0144ska\/KrowodrzaMIESZKANIE NIEUMEBLOWANE - NA \u017bYCZENIE NAJEMCY W\u0141A\u015a",
      |"price":"1250",
      |"title":"przy p\u0119tli. 1pok dla Pary,Studenta.Gnie\u017anie\u0144ska",
      |"image":"https:\/\/otodompl-imagestmp.akamaized.net\/images_otodompl\/32093008_1_184x138_przy-petli-1pok-dla-parystudentagnieznienska-krakow.jpg",
      |"lat":"50.087397",
      |"lng":"19.90752",
      |"exact_location":true,
      |"street_name":"Gnie\u017anie\u0144ska",
      |"promoted":0,
      |"link":"https:\/\/www.otodom.pl\/oferta\/przy-petli-1pok-dla-pary-studenta-gnieznienska-ID3L5V0.html",
      |"param_bottom_1":"1 pok\u00f3j",
      |"param_bottom_2":"pi\u0119tro <strong>7<\/strong> z 10 ",
      |"is_sell":0,
      |"is_rent":1,
      |"param_m":"23.7",
      |"param_price_per_m":53,
      |"param_rooms_num":"1",
      |"currencySymbol":"z\u0142"
      |}
    """.stripMargin

  case class Flat(id: String,
                  price: String,
                  lat: String,
                  lng: String,
                  exact_location: Boolean,
                  street_name: String,
                  param_m: String,
                  param_price_per_m: Double,
                  param_rooms_num: String) {
    def streetNameNice: String = street_name.toLowerCase
      .replace("ul.", "")
      .replace("ul ", "")
      .replace("płk.", "")
      .replace("gen.", "")
      .replace("św.", "")
      .replace("pl.", "")
      .replace("Plac", "")
      .replace("os.", "")
      .replace("os ", "")
      .replace("osiedle", "")
      .replace("dr.", "")
      .replace("al.", "")
      .replace("aleja", "")
      .replaceAll("\\d+", "")
      .trim.split(' ').filter(_.length > 2).last

  }

  object Flat {
    implicit val codec: GenCodec[Flat] = GenCodec.materialize[Flat]
  }

  implicit class FlatsOps(val vec: Vector[Flat]) extends AnyVal {
    def avgPerM: Double = vec.map(_.param_price_per_m).sum / vec.size
  }

  case class Street(name: String, toRent: Vector[Flat], primal: Vector[Flat], secondHand: Vector[Flat]) {
    def primalRatio: Opt[Double] = {
      if (primal.nonEmpty && toRent.nonEmpty) {
        Opt(primal.avgPerM / toRent.avgPerM)
      } else {
        Opt.Empty
      }
    }

    def secondHandRatio: Opt[Double] = {
      if (secondHand.nonEmpty && toRent.nonEmpty) {
        Opt(secondHand.avgPerM / toRent.avgPerM)
      } else {
        Opt.Empty
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val toRent: Vector[Flat] = loadFlats("/home/pawel/dev/mieszkania/wynajem.json")
    val primal = loadFlats("/home/pawel/dev/mieszkania/pierwotny.json")
    val secondHand = loadFlats("/home/pawel/dev/mieszkania/wtorny1.json") ++ loadFlats("/home/pawel/dev/mieszkania/wtorny2.json")
    val primalByStreet = primal.groupBy(_.streetNameNice)
    val secondHandByStreet = secondHand.groupBy(_.streetNameNice)
    val streets = toRent.groupBy(_.streetNameNice).map(v =>
      Street(v._1, v._2, primalByStreet.getOrElse(v._1, Vector.empty), secondHandByStreet.getOrElse(v._1, Vector.empty))).toVector
    println(streets.filter(_.primalRatio.isDefined).sortBy(_.primalRatio.get).map(s => s"${s.name} - ${s.primalRatio.get} - ${s.primal.size}/${s.toRent.size} - ${s.primal.map(s => s.param_price_per_m).mkString(", ")}/${s.toRent.map(s => s.param_price_per_m).mkString(", ")}").mkString("\n"))
    println("\n\n\nsecond hand\n")
    println(streets.filter(_.secondHandRatio.isDefined).sortBy(_.secondHandRatio.get).map(s => s"${s.name} - ${s.secondHandRatio.get} - ${s.secondHand.size}/${s.toRent.size} - ${s.secondHand.map(s => s.param_price_per_m).mkString(", ")}/${s.toRent.map(s => s.param_price_per_m).mkString(", ")}").mkString("\n"))
    //    printData(toRent, "toRent")
    //    printData(primal, "primal")
    //    printData(secondHand, "secondHand")
  }

  def printData(flats: Vector[Flat], name: String): Unit = {
    val byStreet = flats.groupBy(_.streetNameNice)
    println(s"$name - ${flats.size}")
    println(byStreet.mapValues(flats => flats.map(_.param_price_per_m).sum / flats.size).toVector.sortBy(_._2).reverse)
  }

  private def loadFlats(path: String) = {
    val toRentString = Source.fromFile(path).mkString.replace("\\/", "/")
    val toRent = GenCodec.read[Vector[Flat]](new JsonStringInput(new JsonReader(toRentString)))
    toRent
  }
}
