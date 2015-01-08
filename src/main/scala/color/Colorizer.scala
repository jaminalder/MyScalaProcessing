package color

import geometry.Polygon

import scala.annotation.tailrec
import scala.util.Random

object Colorizer {

  val PALETTE_1 = List(Color(0xff468966), Color(0xffFFF0A5), Color(0xffFFB03B), Color(0xffB64926), Color(0xff8E2800))

  def colorizePolygonsRoundRobin(colors:List[Color], polygons:List[Polygon]):List[(Polygon, Color)] = {
    val infiniteColors: Stream[Color] = Stream.continually(colors.toStream).flatten
    polygons zip infiniteColors
  }

  def colorizePolygonsConsiderNeighbors(colors:List[Color], polygons:List[Polygon]):List[(Polygon, Color)] =
    innerColorizePolygons(colors, polygons, Nil)

  @tailrec
  private def innerColorizePolygons(colors:List[Color], notColorized:List[Polygon], colorized:List[(Polygon, Color)]):List[(Polygon, Color)] = {
    notColorized match {
      case Nil => colorized
      case p::ps => {
        val col = colirizePolygon(p, colors, colorized)
        innerColorizePolygons(colors, ps, col::colorized)
      }
    }
  }

  private def colirizePolygon(polygon:Polygon, colors:List[Color], alreadyColored:List[(Polygon,Color)]): (Polygon, Color) = {
    val neighborColors: List[Color] = alreadyColored.filter(cp => polygon.isNeighbor(cp._1)).map(_._2)
    val reducedColors = colors.filterNot(c => neighborColors.contains(c))
    (polygon, reducedColors(Random.nextInt(reducedColors.size)))
  }


}
