import geometry._
import org.scalatest._

class TestsSpec extends FlatSpec with Matchers {

  val v1 = Vec(10.1, 10.3)
  val v2 = Vec(9.8, 10.2)
  val v3 = Vec(10.7, 10.6)
  val v4 = Vec(34.2, 55.9)
  val v5 = Vec(33.6, 56.4)
  val v6 = Vec(88.7, 339.3)
  val v7 = Vec(-88.7, -339.3)

  "Two Vecs" should "be equal if they are similar" in {
    assert(v1 == v2)
    assert(v1 != v3)
  }

  "Two Lines" should "be equal if they are similar" in {
    assert(Line(v1,v4) == Line(v2,v5))
    assert(Line(v1,v4) != Line(v3,v4))
  }

  "Two Polygons" should "be neighbours if they have a common line" in {
    val p1 = Polygon(List(v1,v4,v6))
    val p2 = Polygon(List(v2,v4,v7))
    assert(p1.isNeighbor(p2))
    assert(p1.findNeighbors(List(p2)).size == 1)
  }

  val edges: Polygon = Polygon.regular(5, 200, 0, v1)
  val polys = edges.split(1)

  "A splitted pentagon" should "have 6 sub polygons" in {
    assert(polys.size == 6)
  }

  "A Vec" should "find a point between himself and another Vec" in {
    val v1:Vec = (10,10)
    val v2:Vec = (30,30)
    val pointBetween = v1.pointBetween(0.5, v2)
    assert(Vec(20,20) == pointBetween)

  }

  "A Line" should "find its mid point" in {
    val v1:Vec = (10,10)
    val v2:Vec = (30,30)

    val midPoint = Line(v1,v2).midPoint
    assert(Vec(20,20) == midPoint)

  }

  "A Vec" should "be negatable with the unary operator -" in {
    val v: Vec = Vec(10,10)
    val mv: Vec = -v
    assert(mv.x == -10)
    assert(mv.y == -10)
  }

  "A Vec" should "be rotatable" in {
    val v: Vec = Vec(20,20)
    val rotated180 = v.rotate(Vec(10,10), Math.PI)
    assert(Vec(0,0) == rotated180)
  }

  "A Bezier Curve" should "find its points" in {
    val curve: BezierCurve = BezierCurve(List((10,10), (20,20), (30,30)))
    assert(Vec(10,10) == curve.pointAt(0))
    assert(Vec(30,30) == curve.pointAt(1))
    assert(Vec(20,20) == curve.pointAt(0.5))
  }

}
