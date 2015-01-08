import color.Color
import geometry._
import processing.core.{PConstants, PApplet}

class Drawer(val draw:PApplet) {

  def point5(p:Vec): Unit = draw.ellipse(p.x,p.y,5,5)
  def point5(ps:List[Vec]): Unit = ps.foreach(point5(_))

  def line(l:Line) = draw.line(l.from.x, l.from.y, l.to.x, l.to.y)

  def lines(ls:List[Line]): Unit = ls.foreach(line(_))

  def fillPolygon(coloredPolygon:(Polygon, Color)): Unit = {
    val polygon = coloredPolygon._1
    val color = coloredPolygon._2
    setFillColor(color)
    draw.beginShape()
    polygon.foreach(v => draw.vertex(v.x, v.y))
    draw.endShape(PConstants.CLOSE)
  }

  def setFillColor(c:Color): Unit = {
    draw.fill(c.hexColor)
  }

}
