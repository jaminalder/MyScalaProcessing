import color.Colorizer
import geometry._
import processing.core._

import scala.collection.immutable.IndexedSeq

object GeometryTests extends PApplet {

  private var test:GeometryTests = _

  def main(args: Array[String]) = {
    test = new GeometryTests
    val frame = new javax.swing.JFrame("Test")
    frame.getContentPane().add(test)
    frame.setSize(600,600)
    frame.setVisible(true)
    test.init
  }

}

class GeometryTests extends PApplet {

  val drawer = new Drawer(this)

  override def setup() = {
    size(600,600)
    smooth()
    background(0)
    noFill()

    stroke(255)

    val line = Line((20,20), (100,100))
    val point = Vec(300, 200)

    val mirroredAtPoint = point.mirror((50,50))
    val mirroredAtLine = point.mirror(line)

    drawer.line(line)
    drawer.point5(point)

    stroke(255,0,0)
    drawer.point5(mirroredAtPoint)

    stroke(0,255,0)
    drawer.point5(mirroredAtLine)



  }




}