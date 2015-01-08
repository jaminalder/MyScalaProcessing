import color.Color
import geometry._
import processing.core._

import scala.annotation.tailrec
import scala.util.Random

object CirclesTest extends PApplet {

  private var test:CirclesTest = _

  def main(args: Array[String]) = {
    test = new CirclesTest
    val frame = new javax.swing.JFrame("Test")
    frame.getContentPane().add(test)
    test.init
    frame.setSize(500,500)
    frame.setVisible(true)
  }

}

class CirclesTest extends PApplet {

  val drawer = new Drawer(this)

  override def setup() = {
    size(500,500)
    smooth()
    background(255)

    noStroke()
    noFill()

    val center = (250, 250)

    val edges: Polygon = Polygon.regular(5, 200, 0, center)

    val polys = edges.split(3)

    val colors = List(Color(0xff468966), Color(0xffFFF0A5), Color(0xffFFB03B), Color(0xffB64926), Color(0xff8E2800))

    val colorizedPolygons: List[(Polygon, Color)] = colorizePolygons(colors, polys, Nil)

    colorizedPolygons.foreach(drawer.fillPolygon(_))

    //val lines: List[Line] = polys.map(p => getLines(p)).flatten.distinct

    //lines.foreach(drawer.line(_))



  }

  @tailrec
  private def colorizePolygons(colors:List[Color], notColorized:List[Polygon], colorized:List[(Polygon, Color)]):List[(Polygon, Color)] = {
    notColorized match {
      case Nil => colorized
      case p::ps => {
        val col = colirizePolygon(p, colors, colorized)
        colorizePolygons(colors, ps, col::colorized)
      }
    }
  }

  private def colirizePolygon(polygon:Polygon, colors:List[Color], alreadyColored:List[(Polygon,Color)]): (Polygon, Color) = {
    val neighborColors: List[Color] = alreadyColored.filter(cp => polygon.isNeighbor(cp._1)).map(_._2)
    val reducedColors = colors.filterNot(c => neighborColors.contains(c))
    (polygon, reducedColors(Random.nextInt(reducedColors.size)))
  }







}