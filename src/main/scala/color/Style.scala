package color

class Style(val fill: Boolean = false,
            val fillColor: Color = null,
            val stroke: Boolean = false,
            val strokeColor: Color = null,
            val strokeWeight: Double = 1)

object Style {
  def fill(fillColor: Color): Style = new Style(fill = true, fillColor = fillColor)

  def fillStroke(fillColor: Color, strokeColor: Color): Style =
    new Style(fill = true, fillColor = fillColor, stroke = true, strokeColor = strokeColor)

  def stroke(strokeColor: Color): Style = new Style(stroke = true, strokeColor = strokeColor)
}
