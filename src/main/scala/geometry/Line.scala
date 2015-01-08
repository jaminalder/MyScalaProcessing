package geometry

import scala.annotation.tailrec

class Line(val from:Vec, val to:Vec) {
  val length = from.distance(to)
  val midPoint:Vec = pointOnLine(0.5)
  def pointOnLine(ratio:Double):Vec = from.pointBetween(ratio, to)
  def scale(factor:Double) = Line(from, from.pointBetween(factor, to))
  def prolong(prolongBy:Double) = Line(from, from.pointOnLine(prolongBy, to))

  def canEqual(other: Any): Boolean = other.isInstanceOf[Line]

  override def equals(other: Any): Boolean = other match {
    case that: Line =>
      (that canEqual this) &&
        ((from == that.from && to == that.to) ||
        (from == that.to && to == that.from))
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(from, to)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString(): String = {
    "Line("+from+", "+to+")"
  }

}

object Line {
  def apply(from:Vec, to:Vec) = new Line(from, to)

  def getMidPoints(lines:List[Line]):List[Vec] = lines.map(_.midPoint)

  def getConsecutiveLines(points: List[Vec]):List[Line] = {

    @tailrec
    def consecutiveLinesAcc(accLines:List[Line], ps:List[Vec]):List[Line] = ps match {
        case Nil => accLines
        case p :: Nil => accLines
        case p1 :: p2 :: ps => consecutiveLinesAcc((p1, p2) :: accLines, p2 :: ps)
      }

    consecutiveLinesAcc(Nil,points)
  }

}
