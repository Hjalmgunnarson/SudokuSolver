package com.hjalmgunnarson

import scala.language.postfixOps

class Sudoku() {

  val cells: Seq[ResultCell] = for {
    x <- 1 to 9
    y <- 1 to 9
  } yield ResultCell(x, y, this)

  val layers: Seq[BooleanLayer] = for {index <- 1 to 9} yield BooleanLayer(index)

  def setValue(x: Int, y: Int, value: Int): Unit = layers.foreach(_.setValue(x, y, value))

  def findSolutions(): Seq[ValueCell] = {
    layers.foreach(_.excludeCells())
    (layers.flatMap(_.findSolution()) ++ BooleanLayer.findSoleCandidates(layers)).distinct
  }

  def printSudoku(): Unit = {
    var y = 1
    while (y <= 9) {
      var x = 1
      if ((y - 1) % 3 == 0) println(" -------------------")
      while (x <= 9) {
        if ((x - 1) % 3 == 0) print(" | ")
        print(cells.find(cell => cell.x == x && cell.y == y).get)
        x = x + 1
      }
      println(" |")
      y = y + 1
    }
    println(" -------------------")
  }

  def printCandidates(): Unit = {
    val temp = (for {
      layer <- layers
      cell <- layer.cells
      if cell.value.isEmpty
    } yield ValueCell(cell.x, cell.y, layer.value)).groupBy(cell => cell.x + (cell.y - 1) * 9)
    for {
      (_, values) <- temp.toSeq.sortBy(_._1)
      value <- values
    } println(value)
  }

}

case class ValueCell(x: Int, y: Int, value: Int) {
  override def toString: String = {
    "(" + x + ", " + y + " = " + value + ")"
  }
}

case class ResultCell(x: Int, y: Int, sudoku: Sudoku) {

  def value(): Option[Int] = {
    (for {
      layer <- sudoku.layers
      cell <- layer.getCell(x, y)
      value <- cell.value
      if value
    } yield layer.value).headOption
  }

  override def toString: String = value() match {
    case None => "-"
    case Some(v) => v.toString
  }
}




