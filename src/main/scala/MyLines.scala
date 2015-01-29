import color.{Style, Color, Colorizer}
import geometry._
import processing.core._

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ListBuffer

object MyLines extends PApplet {

  private var test:MyLines = _

  def main(args: Array[String]) = {
    test = new MyLines
    val frame = new javax.swing.JFrame("Test")
    frame.getContentPane().add(test)
    frame.setSize(1700,700)
    frame.setVisible(true)
    test.init
  }

}

class MyLines extends PApplet {

  val drawer = new Drawer(this)

  override def setup() = {
    size(1700,700)
    smooth()
    background(0)
    noFill()

    strokeWeight(0.1)
    noStroke()

    val center = (250, 250)

//    val edges = 5
//    val rounds = 80
//    val initial: Polygon = Polygon.regular(edges, 30, 0, center)
//    val lines = spinLines(initial.lines, edges*rounds)
//    drawer.lines(lines)

    // val circlePoints: List[Vec] = pointsOfPolarFunction(circlePolarFunction(100), 1, 0.1)
    //val spiralPoints: List[Vec] = pointsOfPolarFunction(spiralPolarFunction(3), 10, Math.PI/2)
    //drawer.lines(Line.getConsecutiveLines(spiralPoints.toList.map(_.translate(center))))

    val poly = Polygon.regular(6,150,0,center)

    val innerPolys1: List[Polygon] = innerPolygons(poly, 0.1, 2)

    val triangles: List[Polygon] = innerTriangles(innerPolys1)

    val colorizedPolygons: List[(Polygon, Style)] = Colorizer.fillRoundRobin(Colorizer.PALETTE_4, innerPolys1)
    val colorizedTriangles: List[(Polygon, Style)] = Colorizer.fillRoundRobin(Colorizer.PALETTE_1, triangles)

    //drawer.lines(innerPolys1.map(_.lines).flatten)

    colorizedPolygons.reverse.foreach(drawer.draw(_))
  }

  def innerTriangles(polygons:List[Polygon]):List[Polygon] = polygons match {
    case Nil => Nil
    case p::Nil => Nil
    case outer::inner::ps => {
      val sidePolys: List[Polygon] = outer.innerSidePolygons(inner)
      val innerSidePolys: List[Polygon] = innerTriangles(inner::ps)
      sidePolys:::innerSidePolys
    }
  }

  def innerPolygons(p:Polygon, sideSplitRatio:Double, lineLengthThreshold:Double):List[Polygon] = {

    @tailrec
    def innerPolygonsAcc(accPolys:List[Polygon]): List[Polygon] = {
      if(accPolys.head.lines.head.length < lineLengthThreshold) {
        accPolys
      } else {
        val innerPolygon = accPolys.head.innerPolygon(sideSplitRatio)
        innerPolygonsAcc(innerPolygon::accPolys)
      }
    }

    innerPolygonsAcc(List(p))
  }

  def pointsOfPolarFunction(polarFunction:Double => Double, rounds:Int, stepInRad:Double): List[Vec] = {
    val points: IndexedSeq[Vec] = 0.0 to rounds*2*Math.PI by stepInRad map {w =>
      polarToCartesian(polarFunction, w)
    }
    points.toList
  }

  def circlePolarFunction(r:Double)(w:Double):Double = r
  def spiralPolarFunction(inc:Double)(w:Double):Double = w*inc

  def polarToCartesian(polar:Double => Double, w:Double):Vec = (polar(w) * Math.cos(w), polar(w) * Math.sin(w))


  def spinLines(lines:List[Line], steps:Int):List[Line] = {
    val myLines:ListBuffer[Line] = lines.tail.to[ListBuffer]

    0 to steps-1 foreach {n =>
      val nextLine = Line(myLines.last.to, myLines(n).from).scale(10)
      myLines += nextLine

    }
    val closingLine = Line(myLines.last.to, myLines(steps).from)
    myLines += closingLine

    myLines.toList
  }








}