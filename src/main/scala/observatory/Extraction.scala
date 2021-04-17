package observatory

import java.time.LocalDate

import scala.io.{BufferedSource, Source}

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  type Fahrenheit = Double

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    ???
  }

  def loadSourceFromFile(filePath: String): BufferedSource = Source.fromInputStream(getClass.getResourceAsStream(s"/$filePath"), "utf-8")

  def fahrenheit2Celsius(degreesInF: Fahrenheit): Temperature = (degreesInF - 30.0) * (5/9.0)

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    ???
  }

}
