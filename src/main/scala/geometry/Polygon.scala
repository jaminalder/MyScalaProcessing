package geometry


object Polygon {

  def regular(points: Int, radius: Double, angle: Double, midPoint: Vec): Polygon = {

    def getPolygonPoints(n: Int, num: Int, r: Double, angle: Double, midPoint: Vec): List[Vec] = {
      n match {
        case 0 => Nil;
        case _ => getPolygonPoint(n, num, r, angle, midPoint) :: getPolygonPoints(n - 1, num, r, angle, midPoint)
      }
    }

    def getPolygonPoint(n: Int, num: Int, r: Double, angle: Double, midPoint: Vec): Vec = {
      val x = r * Math.cos(2 * Math.PI * n / num + angle) + midPoint.x
      val y = r * Math.sin(2 * Math.PI * n / num + angle) + midPoint.y
      (x, y)
    }

    new Polygon(getPolygonPoints(points, points, radius, angle, midPoint))
  }
}

case class Polygon(override val points: List[Vec]) extends AbstractShape {
  override type Shape = Polygon

  override def fromPoints(points: List[Vec]): Shape = new Polygon(points)

  val lines: List[Line] = Line.getConsecutiveLines(points :+ points.head)

  def innerPolygon(sideSplitRatio: Double): Polygon = lines.map(_.pointOnLine(sideSplitRatio)).reverse

  def innerSidePolygons(innerPolygon: Polygon): List[Polygon] = {
    val rotatedInnerPoints = innerPolygon.last :: innerPolygon.dropRight(1)
    val result = for (i <- 0 until points.length) yield
      Polygon(List(points(i), innerPolygon(i), rotatedInnerPoints(i)))
    result.toList
  }

  def isNeighbor(other: Polygon): Boolean = other.lines.exists(l => lines.contains(l))

  def findNeighbors(others: List[Polygon]): List[Polygon] = others.filter(isNeighbor(_))

  def split(splitDepth: Int): List[Polygon] = {

    var splitted = List(this)
    1 to splitDepth foreach { _ =>
      splitted = splitted.map(split(_, 0.5)).flatten
    }
    splitted
  }

  private def split(edges: Polygon, ratio: Double): List[Polygon] = {
    val center = edges.midPoint
    val midPoints = edges.lines.map(_.midPoint)
    val innerEdges = midPoints.map(p => p.pointBetween(ratio, center))
    val sidePolys: List[Polygon] = sidePolygons(edges, midPoints, innerEdges)
    innerEdges :: sidePolys
  }

  def sidePolygons(edges: Polygon, midPoints: Polygon, innerEdges: Polygon): List[Polygon] = {
    val rotatedMidPoints = midPoints.last :: midPoints.dropRight(1)
    val rotatedInnerEdges = innerEdges.last :: innerEdges.dropRight(1)
    val result = for (i <- 0 until edges.length) yield
      Polygon(List(edges(i), midPoints(i), innerEdges(i), rotatedInnerEdges(i), rotatedMidPoints(i)))
    result.toList
  }

}
