package com.hjalmgunnarson

import scala.language.postfixOps

object SudokuSolver {
  def main(args: Array[String]): Unit = new Sudoku().go()
}

class Sudoku() {
  // Create the empty layers to store clues
  val layers: Seq[BooleanLayer] = for {
    index <- 1 to 9
  } yield {
    val cells = for {
      x <- 1 to 9
      y <- 1 to 9
    } yield BinaryCell(x, y, None)
    BooleanLayer(index, cells)
  }

  def setValue(x: Int, y: Int, value: Int): Unit = layers.foreach(_.setValue(x, y, value))

  def findSolutions(): Seq[ValueCell] = {
    layers.foreach(_.excludeCells())
    (layers.flatMap(_.findSolution()) ++ BooleanLayer.findSoleCandidates(layers)).distinct
  }

  /**
   * Test
   */
  //  def fillSudoku(): Unit = {
  //
  //    // Tests exlude blockvslinecolumn
  //    setValue(4, 4, 8)
  //    setValue(5, 4, 9)
  //    setValue(6, 4, 4)
  //
  //    setValue(4, 6, 5)
  //    setValue(5, 6, 6)
  //    setValue(6, 6, 7)
  //
  //    setValue(7, 4, 8)
  //    setValue(7, 5, 9)
  //    setValue(7, 6, 4)
  //
  //    setValue(9, 4, 5)
  //    setValue(9, 5, 6)
  //    setValue(9, 6, 7)
  //  }

  //  def fillSudoku(): Unit = {
  //
  //    // Tests findSoleCandidates
  //    setValue(6, 1, 1)
  //    setValue(6, 3, 6)
  //    setValue(4, 4, 4)
  //    setValue(5, 5, 8)
  //    setValue(1, 6, 2)
  //    setValue(3, 6, 9)
  //    setValue(9, 6, 7)
  //    setValue(6, 8, 3)
  //  }

  /**
   * Hardest
   */
  //    def fillSudoku(): Unit = {
  //      setValue(3, 1, 5)
  //      setValue(4, 1, 3)
  //
  //      setValue(1, 2, 8)
  //      setValue(8, 2, 2)
  //
  //      setValue(2, 3, 7)
  //      setValue(5, 3, 1)
  //      setValue(7, 3, 5)
  //
  //      setValue(1, 4, 4)
  //      setValue(6, 4, 5)
  //      setValue(7, 4, 3)
  //
  //      setValue(2, 5, 1)
  //      setValue(5, 5, 7)
  //      setValue(9, 5, 6)
  //
  //      setValue(3, 6, 3)
  //      setValue(4, 6, 2)
  //      setValue(8, 6, 8)
  //
  //      setValue(2, 7, 6)
  //      setValue(4, 7, 5)
  //      setValue(9, 7, 9)
  //
  //      setValue(3, 8, 4)
  //      setValue(8, 8, 3)
  //
  //      setValue(6, 9, 9)
  //      setValue(7, 9, 7)
  //
  //    }

  /**
   * Moeilijk
   */
  def fillSudoku(): Unit = {
    setValue(3, 1, 7)
    setValue(5, 1, 3)
    setValue(8, 1, 8)

    setValue(4, 2, 4)
    setValue(5, 2, 1)
    setValue(6, 2, 7)

    setValue(3, 3, 2)
    setValue(5, 3, 6)
    setValue(9, 3, 4)

    setValue(1, 4, 2)
    setValue(2, 4, 4)
    setValue(6, 4, 6)

    setValue(2, 5, 3)
    setValue(3, 5, 8)
    setValue(6, 5, 4)
    setValue(9, 5, 7)

    setValue(2, 6, 5)
    setValue(7, 6, 9)
    setValue(9, 6, 2)

    setValue(1, 7, 6)
    setValue(2, 7, 2)
    setValue(7, 7, 5)

    setValue(4, 8, 1)
    setValue(5, 8, 9)

    setValue(1, 9, 1)
    setValue(7, 9, 2)
    setValue(9, 9, 9)

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


  def go(): Unit = {
    fillSudoku()
    var solutions = findSolutions()
    layers foreach (_.printLayer())
    while (solutions.nonEmpty) {
      println(solutions)
      solutions.foreach(cell => setValue(cell.x, cell.y, cell.value))
      layers.foreach(_.printLayer())
      solutions = findSolutions()
    }
    //    printCandidates()
    println("Done")
  }

}

case class BinaryCell(x: Int, y: Int, var value: Option[Boolean]) {
  def setValue(v: Boolean): Unit = value = Some(v)

  def hasCoordinates(x: Int, y: Int): Boolean = this.x == x && this.y == y
}

case class ValueCell(x: Int, y: Int, value: Int) {
  override def toString: String = {
    "(" + x + ", " + y + " = " + value + ")"
  }
}



