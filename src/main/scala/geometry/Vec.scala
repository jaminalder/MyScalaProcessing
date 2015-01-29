package geometry

import org.apache.commons.math3.geometry.euclidean.twod.Vector2D

import scala.util.Random

class Vec(val x: Double, val y: Double) {
  val v2d = new Vector2D(x, y)
  private val roundedX: Long = x.round
  private val roundedY: Long = y.round

  def +(v2: Vec): Vec = v2d.add(v2.v2d)

  def -(v2: Vec): Vec = v2d.subtract(v2.v2d)

  def unary_-(): Vec = v2d.negate()

  def *(s: Double): Vec = v2d.scalarMultiply(s)

  def rotate(center: Vec, angle: Double): Vec = {
    val norm: Vec = this - center
    val cos: Double = Math.cos(angle)
    val sin: Double = Math.sin(angle)
    val rotatedX = norm.x * cos - norm.y * sin
    val rotatedY = norm.x * sin + norm.y * cos
    Vec(rotatedX, rotatedY) + center
  }

  def midPoint(v2: Vec): Vec = pointBetween(0.5, v2)

  def pointBetween(ratio: Double, v2: Vec): Vec = {
    val r1 = ratio
    val r2 = 1 - ratio
    val px = (r1 * v2.x + r2 * x)
    val py = (r1 * v2.y + r2 * y)
    new Vec(px, py)
  }

  def distance(v2: Vec): Double = v2d.distance(v2.v2d)

  def pointOnLine(prolongBy: Double, endPoint: Vec): Vec = {
    val scale = 1 + (prolongBy / distance(endPoint))
    pointBetween(scale, endPoint)
  }

  def mirror(mirrorPoint:Vec):Vec = {
    val connectionVec = mirrorPoint - this
    this + (connectionVec * 2)
  }

  def mirror(mirrorLine:Line):Vec = mirror(closestPointOnLine(mirrorLine))

  def closestPointOnLine(line:Line):Vec = {
    // according to http://stackoverflow.com/a/3121418 without restricting to line
    val ab: Vec = line.from-line.to
    val k: Double = (this-line.to).v2d.dotProduct(ab.v2d)  /  ab.v2d.dotProduct(ab.v2d)
    line.from * k + line.to * (1-k)
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

  override def toString(): String = {
    "(" + roundedX + "," + roundedY + ")"
  }

}

object Vec {
  def apply(x: Double, y: Double) = new Vec(x, y)

  def random(corner1: Vec, corner2: Vec) = {
    val x = corner1.x + Random.nextDouble() * (corner2.x - corner1.x)
    val y = corner1.y + Random.nextDouble() * (corner2.y - corner1.y)
    Vec(x, y)
  }
}
