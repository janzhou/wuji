package wuji

case class SLocation(start:Int = 0, end:Int = 0) {
  def +(loc:SLocation) = SLocation(Math.min(start, loc.start), Math.max(end, loc.end))
}

object SLocation {
  def apply():SLocation = SLocation(0, 0)
}

abstract class SExp extends SObject {
  val location:SLocation
}

case class SList(location:SLocation, values:List[SObject]) extends SExp {
  val names = List("SList")
  def length = values.length
  def value(id:Int) = {
    if(id < 0 || id >= values.length) throw RuntimeException(location, s"not such exp in list ${this} at $id")
    values(id)
  }

  override def toString = s"(${values.mkString(" ")})"
}

object SList extends {
  def apply(location:SLocation):SList = SList(location, List[SExp]())
  def apply(a:SLocation, b:SLocation):SList = apply(a + b)
  def apply(values:List[SObject]):SList = {
    val location = if(values.length > 0) {
      values.map(_.location).reduce(_ + _)
    } else SLocation()

    SList(location, values)
  }
}

case class SSymbol(location:SLocation, value:String) extends SExp {
  val names = List("SSymbol")
  override def toString = value.toString()
}

object SSymbol {
  def apply(value:String):SSymbol = SSymbol(SLocation(), value)
}

case class SExit(location:SLocation) extends SExp {
  val names = List("SExit")
  override def toString = "#exit"
}