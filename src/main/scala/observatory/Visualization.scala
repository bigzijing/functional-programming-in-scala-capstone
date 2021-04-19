package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import math.{abs, acos, cos, pow, sin, toRadians, round}
import scala.annotation.tailrec

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {
  val idwPower = 2
  val earthRadiusKM = 6371

  val imageWidth = 360
  val imageHeight = 180

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
    else calcIDW(distances)
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
    val colorsSorted = points.toList.sortBy(_._1)

    @tailrec
    def interpolateRecursively(points: List[(Temperature, Color)], value: Temperature): Color = points match {
      case left :: right :: _ if (left._1 < value && right._1 > value) =>
        val interpolatedR = round((left._2.red + left._2.red) / 2.0).toInt
        val interpolatedG = round((left._2.green + left._2.green) / 2.0).toInt
        val interpolatedB = round((left._2.blue + left._2.blue) / 2.0).toInt

        Color(interpolatedR, interpolatedG, interpolatedB)
      case _ :: right :: tail => interpolateRecursively(right :: tail, value)
      case head :: Nil => head._2
    }

    if (colorsSorted.head._1 > value) colorsSorted.head._2
    else if (colorsSorted.last._1 < value) colorsSorted.last._2
    else {
      interpolateRecursively(colorsSorted, value)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)],
                colors: Iterable[(Temperature, Color)]): Image = {
    def convertArrayIndexToLocation(index: Int): Location = {
      val xPoint: Int = index % imageWidth
      val yPoint: Int = index / imageHeight

      Location(90 - yPoint, xPoint - 180)
    }

    val pixels = (0 until imageHeight * imageWidth).map { index =>
      val rgb = interpolateColor(colors, predictTemperature(temperatures, convertArrayIndexToLocation(index)))
      Pixel(rgb.red, rgb.green, rgb.blue, 255)
    }.toArray

    Image(imageWidth, imageHeight, pixels)
  }

}
