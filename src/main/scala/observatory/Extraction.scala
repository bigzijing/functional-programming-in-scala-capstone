package observatory

import java.time.LocalDate
import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {
  type Fahrenheit = Double
  case class Station(STN: String, WBAN: String)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val temperatureFileIterable: Iterator[String] = loadSourceFromFile(temperaturesFile)
    val stationsFileIterable: Iterator[String] = loadSourceFromFile(stationsFile)

    val temperatureByStation: Iterator[(Station, LocalDate, Temperature)] = temperatureFileIterable.map(_.split(",")).collect {
      case Array(stn, wban, m, d, f) => (Station(stn, wban), LocalDate.of(year, m.toInt, d.toInt), fahrenheit2Celsius(f.toDouble))
    }
    val stationsToLocationMap: Map[Station, Location] = stationsFileIterable.map(_.split(",")).collect {
      case Array(stn, wban, lat, long) if validGPS(lat, long) => (Station(stn, wban), Location(lat.toDouble, long.toDouble))
    }.toMap

    temperatureByStation.collect {
      case line if stationsToLocationMap.contains(line._1) => (line._2, stationsToLocationMap(line._1) ,line._3)
    }.toIterable
  }

  def validGPS(lat: String, long: String): Boolean = !List(lat, long).contains("")

  def loadSourceFromFile(fileName: String): Iterator[String] = Source.fromInputStream(getClass.getResourceAsStream(fileName)).getLines()

  def fahrenheit2Celsius(degreesInF: Fahrenheit): Temperature = (degreesInF.toDouble - 32.0) * (5.0/9.0)

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.groupBy(_._2).map { iter =>
      val location = iter._1
      val tempIter = iter._2.map(_._3)
      (location, tempIter.sum / tempIter.size)
    }
  }

}
