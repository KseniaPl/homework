import java.io.{File, PrintWriter}
import scala.io.Source
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer

case class Country (
                     area: Int,
                     name: String,
                     capital: String
                   )


object MyApp extends App{
  val source = Source.fromURL("https://raw.githubusercontent.com/mledoze/countries/master/countries.json").mkString
  implicit val formats = DefaultFormats

  val x = parseJson

//  implicit val orderer = Ordering.Double.TotalOrdering
  val res = ListMap(x.toSeq.sortWith(_._1 > _._1):_*).take(10)

  val jstr = createJson

  val printWriter = new PrintWriter(new File(args(0)))
  printWriter.println(jstr)
  printWriter.flush()
  printWriter.close()


  private def createJson: String = {
    var jList = new ListBuffer[Country]()
    for ((k, v) <- res) {
      val country = Country(
        area = k,
        name = v(0),
        capital = v(1)
      )
      jList += country
    }
    val jstr = write(jList)
    return jstr
  }



  private def parseJson:Map[Int, Array[String]]  = {
    var x:Map[Int, Array[String]] = Map()
    parse(source).children.map { child =>
      val region: String = (child \ "region").extract[String]
      if (region == "Africa") {
        val area: Int = (child \ "area").extract[Int]
        val name: String = (child \ "name" \ "common").extract[String]
        val capital_arr = (child \ "capital")
        val capital = if (capital_arr.children.length > 0) capital_arr(0).extract[String] else ""
        x += (area -> Array(name, capital))
      }
    }
    return x
  }
}
