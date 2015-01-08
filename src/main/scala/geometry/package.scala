import org.apache.commons.math3.geometry.euclidean.twod.Vector2D

package object geometry {

  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }
  def roundAt2(n:Double) = roundAt(2)(n)
  def roundAt0(n:Double) = roundAt(0)(n)

  implicit def doubleToFloat(d:Double):Float = d.toFloat

  implicit class Vec2DVec(val v:Vector2D) extends Vec(v.getX, v.getY)
  implicit class DoubleVec(xy:(Double, Double)) extends Vec(xy._1,xy._2)
  implicit class IntVec(xy:(Int, Int)) extends Vec(xy._1,xy._2)

  implicit def vecTupleToLine(vv:(Vec,Vec)) = Line(vv._1, vv._2)
  implicit def vecListToPolygon(vl:List[Vec]) = Polygon(vl)
  implicit def polygonToVecList(p:Polygon) = p.points

}
