package geometry

import scala.annotation.tailrec

case class Line(val from: Vec, val to: Vec) extends AbstractShape {
  override type Shape = Line
  override val points: List[Vec] = List(from, to)

  override def fromPoints(points: List[Vec]): Line = new Line(points(0), points(1))

  val length = from.distance(to)

  def pointOnLine(ratio: Double): Vec = from.pointBetween(ratio, to)

}

object Line {

  def getConsecutiveLines(points: List[Vec]): List[Line] = {

    @tailrec
    def consecutiveLinesAcc(accLines: List[Line], ps: List[Vec]): List[Line] = ps match {
      case Nil => accLines
      case p :: Nil => accLines
      case p1 :: p2 :: pss => consecutiveLinesAcc(Line(p1, p2) :: accLines, p2 :: pss)
    }

    consecutiveLinesAcc(Nil, points).reverse
  }
}

