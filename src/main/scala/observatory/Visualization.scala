package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math.{abs}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)],
                         location: Location): Temperature = {
    val distances =
      temperatures.map(row => (calcDistance(row._1, location), row._2))
  }

  def calcDistance(loc1: Location, loc2: Location): Double = {
    if (loc1 == loc2) 0
    else if (areAntipodes(loc1, loc2)) {
      ???
    } else {
      ???
    }
  }

  def areAntipodes(loc1: Location, loc2: Location): Boolean =
    (loc1.lat == -loc2.lat) && (abs(loc1.lon - loc2.lon) == 180)

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
