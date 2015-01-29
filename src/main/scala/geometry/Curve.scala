package geometry

case class Curve(val points: List[Vec]) extends AbstractShape {
  override type Shape = Curve
  override def fromPoints(points:List[Vec]) = new Curve(points)


}