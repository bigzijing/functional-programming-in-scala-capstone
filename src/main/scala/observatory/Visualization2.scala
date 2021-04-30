package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    val x = point.x
    val y = point.y
    (d00 * (1 - x) * (1 - y)) + (d10 * x * (1 - y)) + (d01 * (1 - x) * y) + (d11 * x * y)
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    val imageHeight = 256
    val imageWidth = 256

    val coords = for {
      i <- 0 until imageHeight
      j <- 0 until imageWidth
    } yield (i, j)

    val imagePixels = coords
      .toArray
      .map {
        case (y, x) =>
          Tile(
            tile.x * imageWidth + x,
            tile.y * imageHeight + y,
            tile.zoom + 8
          ) }
      .map(Interaction.tileLocation)
      .map { loc =>
        val latVal = loc.lat.toInt
        val lonVal = loc.lon.toInt
        bilinearInterpolation(
          CellPoint(loc.lat - latVal, loc.lon - lonVal),
          grid(GridLocation(latVal, lonVal)),
          grid(GridLocation(latVal + 1, lonVal)),
          grid(GridLocation(latVal, lonVal + 1)),
          grid(GridLocation(latVal + 1, lonVal + 1))
        )
      }
      .map(Visualization.interpolateColor(colors, _))
      .map(color => Pixel(color.red, color.green, color.blue, 127))

    Image(imageWidth, imageHeight, imagePixels)
  }

}
