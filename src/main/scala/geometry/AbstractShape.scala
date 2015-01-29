package geometry

abstract class AbstractShape {
  type Shape <: AbstractShape
  val points: List[Vec]

  def fromPoints(points: List[Vec]): Shape

  def translate(v: Vec): Shape = mapOnPoints(_ + v)

  def scale(s: Double): Shape = mapOnPoints(_ * s)

  def rotate(center:Vec, angle:Double): Shape = mapOnPoints(_.rotate(center, angle))

  private def mapOnPoints(f:Vec => Vec): Shape = fromPoints(points.map(f))

  lazy val midPoint: Vec = {
    val x = points.map(_.x).sum / points.size
    val y = points.map(_.y).sum / points.size
    Vec(x, y)
  }



}
