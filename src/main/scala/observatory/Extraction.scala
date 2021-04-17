package observatory

import java.time.LocalDate

import scala.io.{BufferedSource, Source}

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
    val yearFileIterable: Iterable[String] = loadSourceFromFile(year.toString)
    val stationsFileIterable: Iterable[String] = loadSourceFromFile(stationsFile)

    val temperatureByStation: Iterable[(Station, LocalDate, Temperature)] = yearFileIterable.map(_.split(",")).collect {
      case Array(stn, wban, m, d, f) => (Station(stn, wban), LocalDate.of(year, m.toInt, d.toInt), fahrenheit2Celsius(f.toDouble))
    }
    val stationsToLocationMap: Map[Station, Location] = stationsFileIterable.map(_.split(",")).collect {
      case Array(stn, wban, lat, long) => (Station(stn, wban), Location(lat.toDouble, long.toDouble))
    }.toMap

    temperatureByStation.collect {
      case line if stationsToLocationMap.contains(line._1) => (line._2, stationsToLocationMap(line._1) ,line._3)
    }
  }

  def loadSourceFromFile(filePath: String): Iterable[String] = Source.fromInputStream(getClass.getResourceAsStream(s"/$filePath"), "utf-8").getLines().toIterable

  def fahrenheit2Celsius(degreesInF: Fahrenheit): Temperature = (degreesInF - 30.0) * (5/9.0)

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    ???
  }

}
