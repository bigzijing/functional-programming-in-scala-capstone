package observatory

import Visualization._
import com.sksamuel.scrimage.{Image, Pixel}
import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  val tileHeight = 256
  val tileWidth = 256

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    Location(
      toDegrees(atan(sinh(Pi * (1.0 - 2.0 * tile.y.toDouble / pow(2, tile.zoom))))),
      tile.x.toDouble / pow(2, tile.zoom) * 360.0 - 180.0)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val coords = for {
      i <- 0 until tileHeight
      j <- 0 until tileWidth
    } yield (i, j)

    val tilePixels = coords
      .map {
        case (y, x) =>
          Tile(
            tile.x * tileWidth + x,
            tile.y * tileHeight + y,
            tile.zoom + 8
          ) }
      .map(tileLocation)
      .map(predictTemperature(temperatures, _))
      .map(interpolateColor(colors, _))
      .map(color => Pixel(color.red, color.green, color.blue, 127))
      .toArray

    Image(tileWidth, tileHeight, tilePixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for {
      zoom <- 0 to 3
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2, zoom).toInt
      data <- yearlyData
    } yield generateImage(data._1, Tile(x, y, zoom), data._2)
  }

}
