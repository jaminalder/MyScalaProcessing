package color

import geometry.Polygon

import scala.annotation.tailrec
import scala.util.Random

object Colorizer {

  val WHITE = Color(0xffffffff)
  val BLACK = Color(0xff000000)


  val PALETTE_1 = List(Color(0xff468966), Color(0xffFFF0A5), Color(0xffFFB03B), Color(0xffB64926), Color(0xff8E2800))
  val PALETTE_1_T = List(Color(0x88468966), Color(0x88FFF0A5), Color(0x88FFB03B), Color(0x88B64926), Color(0x888E2800))
  val PALETTE_2 = List(Color(0xff2E0927), Color(0xffD90000), Color(0xffFF2D00), Color(0xffFF8C00), Color(0xff04756F))
  val PALETTE_3 = List(Color(0xff96CA2D), Color(0xffB5E655), Color(0xffEDF7F2), Color(0xff4BB5C1), Color(0xff7FC6BC))
  val PALETTE_4 = List(Color(0xffCC4452), Color(0xffE6B098), Color(0xffF9E4AD), Color(0xff723147), Color(0xff31152B))
  val PALETTE_BW = List(Color(0xffffffff), Color(0xff000000))

  def fillRoundRobin[T](colors:List[Color], objects:List[T]):List[(T, Style)] = {
    val styles = colors.map(Style.fill(_))
    val infiniteColors: Stream[Style] = Stream.continually(styles.toStream).flatten
    objects zip infiniteColors
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
