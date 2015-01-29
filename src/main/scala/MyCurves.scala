import color.Colorizer
import geometry._
import processing.core._

import scala.collection.immutable.IndexedSeq

object MyCurves extends PApplet {

  private var test:MyCurves = _

  def main(args: Array[String]) = {
    test = new MyCurves
    val frame = new javax.swing.JFrame("Test")
    frame.getContentPane().add(test)
    frame.setSize(600,600)
    frame.setVisible(true)
    test.init
  }

}

class MyCurves extends PApplet {

  val drawer = new Drawer(this)

  override def setup() = {
    size(600,600)
    smooth()
    background(0)
    noFill()

    stroke(255)


    val startPoint: Vec = (0,0)
    val endPoint: Vec = (0,100)
    val upperDiff: Vec = (10,75)
    val lowerDiff: Vec = (30,0)

    val rainDrop = BezierCurve(List(
      startPoint,
      startPoint+upperDiff,
      endPoint+lowerDiff,
      endPoint,
      endPoint-(lowerDiff.x, -lowerDiff.y),
      startPoint-(upperDiff.x, -upperDiff.y),
      startPoint
    ))


    val s: BezierCurve = rainDrop.scale(2).translate((100,100))
    val ss = s.scale(2)

    val splitted = ss.splitAt(0.25)

    var drop2 = splitted._1
    //drop2 = BezierCurve(drop2.points.last::drop2.points.head + (50,-20)::drop2.points)

    val topPoint = drop2.points.head + (150,-20)

    val mirroredPoints = drop2.points.map(p => p.mirror(Line(drop2.points.last,topPoint)))

    drop2 = BezierCurve(drop2.points.reverse ++ (topPoint::mirroredPoints))

    drawer.draw(ss)

    stroke(255,0,0)
    drawer.draw(drop2)

    stroke(0,255,0)
    //drawer.draw(splitted._2)


/*
    fill(255,0,0)
    val pointsOnCurve: List[Vec] = (for(i <- 0.0 to 1.0 by 0.25) yield s.pointAt(i)).toList
    pointsOnCurve.foreach{x =>
      println(x)
      drawer.point5(x)
    }
*/

    fill(0,255,0)
    drop2.points.foreach(drawer.point5(_))


  }




}