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
      val loc1InRads = Location(lat = toRadians(loc1.lat), lon = toRadians(loc1.lon))
      val loc2InRads = Location(lat = toRadians(loc2.lat), lon = toRadians(loc2.lon))
      val chgInLonRadians = abs(loc1InRads.lon - loc2InRads.lon)

      earthRadiusKM * acos(sin(loc1InRads.lat) * sin(loc2InRads.lat) + cos(loc1InRads.lat) * cos(loc2InRads.lat) * cos(chgInLonRadians))
    }
  }

  def areAntipodes(loc1: Location, loc2: Location): Boolean =
    (loc1.lat == -loc2.lat) && (abs(loc1.lon - loc2.lon) == 180)

  def calcIDW(temperaturesByDistance: Iterable[(Double, Temperature)]): Double = {
    val (weightedSum, inverseWeightedSum) = temperaturesByDistance
      .foldLeft((0.0, 0.0)) {
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
    points.find(_._1 == value) match {
      case Some(point) => point._2
      case _ =>
        val (lowerPoints, higherPoints) = points.partition(_._1 < value)
        if (lowerPoints.isEmpty) higherPoints.minBy(_._1)._2
        else if (higherPoints.isEmpty) lowerPoints.maxBy(_._1)._2
        else {
          val leftPoint = lowerPoints.maxBy(_._1)
          val rightPoint = higherPoints.minBy(_._1)

          def interpolationRatio(value: Double, left: Double, right: Double): Double = {
            val range = right - left
            val valueRange = value - left
            valueRange / range
          }

          val ratio = interpolationRatio(value, leftPoint._1, rightPoint._1)

          def interpolateColor(leftColor: Int, rightColor: Int): Int = {
            val leftRightDiff = rightColor - leftColor
            leftColor + (ratio * leftRightDiff).round.toInt
          }

          Color(
            interpolateColor(leftPoint._2.red, rightPoint._2.red),
            interpolateColor(leftPoint._2.green, rightPoint._2.green),
            interpolateColor(leftPoint._2.blue, rightPoint._2.blue))
        }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)],
                colors: Iterable[(Temperature, Color)]): Image = {
    def convertIndexToLocation(index: (Int, Int)): Location = {
      val (xCoord, yCoord) = index
      val lat = -(xCoord - (imageHeight/2)) * (180/imageHeight)
      val lon = (yCoord - imageWidth/2) * (360/imageWidth)
      Location(lat, lon)
    }

    val coords = for {
      i <- 0 until imageHeight
      j <- 0 until imageWidth
    } yield (i, j)

    val pixels = coords.map { index =>
      val rgb = interpolateColor(colors, predictTemperature(temperatures, convertIndexToLocation(index)))
      Pixel(rgb.red, rgb.green, rgb.blue, 255)
    }.toArray

    Image(imageWidth, imageHeight, pixels)
  }

}
