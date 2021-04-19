package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math.{abs, toRadians, acos, sin, cos, pow}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {
  val idwPower = 2
  val earthRadiusKM = 6371

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)],
                         location: Location): Temperature = {
    val distances = temperatures.map(row => (calcDistance(row._1, location), row._2))
    val distancesMinByDistance = distances.minBy(_._1)

    if (distancesMinByDistance._1 < 1) distancesMinByDistance._2
    else {
      ???
    }
  }

  def calcDistance(loc1: Location, loc2: Location): Double = {
    if (loc1 == loc2) 0
    else if (areAntipodes(loc1, loc2)) {
      earthRadiusKM * math.Pi
    } else {
      val loc1InRads =
        Location(lat = toRadians(loc1.lat), lon = toRadians(loc1.lon))
      val loc2InRads =
        Location(lat = toRadians(loc2.lat), lon = toRadians(loc2.lon))
      val chgInLonRadians = abs(loc1InRads.lon - loc2InRads.lon)

      acos(sin(loc1InRads.lat) * sin(loc2InRads.lat) + cos(loc1InRads.lat) * cos(loc2InRads.lat) * cos(chgInLonRadians))
    }
  }

  def areAntipodes(loc1: Location, loc2: Location): Boolean =
    (loc1.lat == -loc2.lat) && (abs(loc1.lon - loc2.lon) == 180)

  def calcIDW(temperaturesByDistance: Iterable[(Double, Temperature)]): Double = {
    val (weightedSum, inverseWeightedSum) = temperaturesByDistance
      .foldLeft(0.0, 0.0) {
        case ((weightAcc, inverseWeightAcc), (dist, temp)) =>
          val weight = 1 / pow(dist, idwPower)
          (weight * temp + weightAcc, weight + inverseWeightAcc)
      }
    weightedSum / inverseWeightedSum
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)],
                       value: Temperature): Color = {
    ???
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)],
                colors: Iterable[(Temperature, Color)]): Image = {
    ???
  }

}
