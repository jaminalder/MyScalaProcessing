package geometry

import org.apache.commons.math3.geometry.euclidean.twod.Vector2D

class Vec(val x:Double, val y:Double) {
  val v2d = new Vector2D(x,y)
  private val roundedX:Long = x.round
  private val roundedY:Long = y.round
  def translate(t:Vec): Vec = new Vec(x+t.x,y+t.y)
  def +(v2:Vec): Vec = v2d.add(v2.v2d)
  def midPoint(v2:Vec): Vec = pointBetween(0.5, v2)
  def pointBetween(ratio:Double, v2:Vec): Vec = {
    val r1 = ratio
    val r2 = 1-ratio
    val px = (r1*v2.x + r2*x)
    val py = (r1*v2.y + r2*y)
    new Vec(px,py)
  }
  def distance(v2:Vec):Double = v2d.distance(v2.v2d)

  def pointOnLine(prolongBy:Double, endPoint:Vec): Vec = {
    val scale = 1 + (prolongBy / distance(endPoint))
    pointBetween(scale, endPoint)
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Vec]

  override def equals(other: Any): Boolean = other match {
    case that: Vec =>
      (that canEqual this) &&
        roundedX == that.roundedX &&
        roundedY == that.roundedY
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(roundedX, roundedY)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString():String = {
    "("+roundedX+","+roundedY+")"
  }

}

object Vec {
  def apply(x:Double, y:Double) = new Vec(x,y)
}
