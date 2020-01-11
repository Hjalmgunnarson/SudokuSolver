package com.hjalmgunnarson

case class BinaryCell(x: Int, y: Int, var value: Option[Boolean]) {
  def setValue(v: Boolean, layer: Int = 0, hint: String = ""): Unit = {
    value match {
      case None => value = Some(v)
      case Some(value) if value != v =>
        throw new Exception(s"Trying to set layer $layer x = $x, y = $y to $v, but was already set to $value in $hint")
      case _ => ()
    }
  }
  def hasCoordinates(x: Int, y: Int): Boolean = this.x == x && this.y == y

  override def toString: String = value match {
    case None => "-"
    case Some(true) => "1"
    case Some(false) => "0"
  }
}
