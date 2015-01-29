import color.{Style, Color}
import geometry._
import processing.core.{PApplet, PConstants}

import scala.annotation.tailrec

class Drawer(val draw:PApplet) {

  def point5(p:Vec): Unit = draw.ellipse(p.x,p.y,5,5)

  def line(l:Line) = draw.line(l.from.x, l.from.y, l.to.x, l.to.y)

  def draw(styledShape: (AbstractShape, Style)): Unit ={
    setStyle(styledShape._2)
    draw(styledShape._1)
  }

  def draw(shape: AbstractShape): Unit = shape match {
    case p:Polygon => polygon(p)
    case b:BezierCurve => bezierCurve(b)
    case _ => throw new IllegalArgumentException("Undrawable shape type: " + shape.getClass.getSimpleName)
  }

  def setStyle(style:Style): Unit ={
    if(style.fill){
      draw.fill(style.fillColor.hexColor)
    }else{
      draw.noFill()
    }

    if(style.stroke) {
      draw.stroke(style.strokeColor.hexColor)
      draw.strokeWeight(style.strokeWeight)
    } else {
      draw.noStroke()
    }
  }

  def polygon(polygon: Polygon) {
    draw.beginShape()
    polygon.foreach(v => draw.vertex(v.x, v.y))
    draw.endShape(PConstants.CLOSE)
  }

  def curve(c:Curve): Unit ={
    draw.beginShape()
    draw.curveVertex(c.points.head.x, c.points.head.y)
    c.points.foreach(p => draw.curveVertex(p.x, p.y))
    draw.curveVertex(c.points.last.x, c.points.last.y)
    draw.endShape()
  }

  def bezierCurve(b:BezierCurve): Unit = {



    @tailrec
    def bezierVertex(points:List[Vec]): Unit = points match {
      case p1::ps => {
        draw.curveVertex(p1.x, p1.y)
        bezierVertex(ps)
      }
      case _ =>
    }


    val pointsOnCurve: List[Vec] = (for(i <- 0.0 to 1.0 by 0.05) yield b.pointAt(i)).toList

    draw.beginShape()
    draw.curveVertex(pointsOnCurve.head.x, pointsOnCurve.head.y)
    pointsOnCurve.foreach(p => draw.curveVertex(p.x, p.y))
    draw.curveVertex(pointsOnCurve.last.x, pointsOnCurve.last.y)
    draw.endShape()
  }
}
