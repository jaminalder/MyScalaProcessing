import geometry.Vec

abstract class MyAbstractShape(val points:List[Vec]) {
  type Shape <: MyAbstractShape
  def fromPoints(points:List[Vec]):Shape
  def translate(v:Vec):Shape = fromPoints(points.map(_ + v))
  def scale(s:Double):Shape = fromPoints(points.map(_ * s))
}

case class MyLine(from:Vec,to:Vec) extends MyAbstractShape(List(from,to)) {
  override type Shape = MyLine
  override def fromPoints(points: List[Vec]): Shape = MyLine(points(0),points(1))
}

val l = MyLine((20,20), (50,50))

val ll = l.translate((20,50))

val v = Vec(20,20)
val mv = -v