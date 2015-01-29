package geometry

import org.apache.commons.math3.util.ArithmeticUtils

case class BezierCurve(val points: List[Vec]) extends AbstractShape {
  override type Shape = BezierCurve
  override def fromPoints(points:List[Vec]) = new BezierCurve(points)

  val n = points.size-1

  def pointAt(t:Double):Vec = casteljauFunction(Line.getConsecutiveLines(points), t)

  def pointX(t:Double) = bezierFunction(0,t, _.x)
  def pointY(t:Double) = bezierFunction(0,t, _.y)
  
  def bezierFunction(i:Int, t:Double, fXY:Vec => Double):Double = {
    if(i>n){
      0
    } else {
      val binomialTerm = ArithmeticUtils.binomialCoefficient(n,i)
      val polynomialTerm = Math.pow(1-t,n-i) * Math.pow(t,i)
      val weight = fXY(points(i))
      val result = binomialTerm * polynomialTerm * weight
      result + bezierFunction(i+1,t,fXY)
    }
  }

  def casteljauFunction(lines:List[Line], t:Double):Vec = {
    if(lines.size == 1) {
      lines.head.pointOnLine(t)
    } else {
      val newPoints = lines.map(_.pointOnLine(t))
      val newLines = Line.getConsecutiveLines(newPoints)
      casteljauFunction(newLines, t)
    }
  }

  def splitAt(t:Double):(BezierCurve,BezierCurve) = {
    val newPoints = casteljauSplit(Line.getConsecutiveLines(points), t)
    (BezierCurve(points.head::newPoints._1),BezierCurve((points.last::newPoints._2).reverse))
  }

  def casteljauSplit(lines:List[Line], t:Double):(List[Vec],List[Vec]) = {
    if(lines.size == 1) {
      val p = lines.head.pointOnLine(t)
      (List(p), List(p))
    } else {
      val newPoints = lines.map(_.pointOnLine(t))
      val newLines = Line.getConsecutiveLines(newPoints)
      val split = casteljauSplit(newLines, t)
      (newPoints.head::split._1,newPoints.last::split._2)
    }
  }

  def toCurve(stepSize:Double):Curve = {
    val pointsOnCurve: List[Vec] = (for(i <- 0.0 to 1.0 by stepSize) yield pointAt(i)).toList
    Curve(pointsOnCurve)
  }


}