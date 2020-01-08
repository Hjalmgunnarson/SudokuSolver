package com.hjalmgunnarson

import scala.language.postfixOps
case class BooleanLayer(index: Int, cells: List[BinaryCell]) {
  // Lines contains the horizontal lists of cells.
  val lines: Map[Int, List[BinaryCell]] = 1 to 9 map (i => (i, cells.filter(_.y == i))) toMap
  // Columns contains the vertical lists of cells.
  val columns: Map[Int, List[BinaryCell]] = 1 to 9 map (i => (i, cells.filter(_.x == i))) toMap
  // Block contains the lists of cells grouped by block
  val blocks: Map[Int, List[BinaryCell]] = 1 to 9 map (i => (i, cells.filter(cell => getBlockId(cell.x, cell.y) == i))) toMap

  // Each line is divided into three lists of three cells.
  val lineParts: List[ListPart] = (for {
    lineIndex <- 1 to 9
    blockIndex <- 1 to 3
  } yield ListPart(lineIndex, blockIndex, cells.filter(cell => blockIndex == mod(cell.x) && lineIndex == cell.y))) toList

  // Each column is divided into three lists of three cells.
  val columnParts: List[ListPart] = (for {
    columnIndex <- 1 to 9
    blockIndex <- 1 to 3
  } yield ListPart(columnIndex, blockIndex, cells.filter(cell => columnIndex == cell.x && blockIndex == mod(cell.y)))) toList

  // All parts of the lines are grouped by the block they're in
  val linePartsPerBlock: List[List[ListPart]] = (for {
    vertBlockIndex <- 1 to 3
    horBlockIndex <- 1 to 3
  } yield lineParts.filter(part => mod(part.index) == vertBlockIndex && part.partIndex == horBlockIndex)) toList

  // All parts of the columns are grouped by the block they're in
  val columnPartsPerBlock: List[List[ListPart]] = (for {
    horBlockIndex <- 1 to 3
    vertBlockIndex <- 1 to 3
  } yield columnParts.filter(part => mod(part.index) == horBlockIndex && part.partIndex == vertBlockIndex)) toList

  // Sets value in designated cell and excludes cells with the same coordinates on the other layers
  // Excludes cells in the same line, column and block on this layer
  def setValue(x: Int, y: Int, value: Int): Unit = {
    cells.find(_.hasCoordinates(x, y)).foreach(_.setValue(value == index))
    if (this.index == value) {
      for {cell <- lines(y) if cell.x != x} cell.setValue(false) // all other cells on this line cannot hold the number
      for {cell <- columns(x) if cell.y != y} cell.setValue(false) // all other cells in this column cannot hold the number
      for {cell <- blocks(getBlockId(x, y)) if cell.x != x && cell.y != y} cell.setValue(false) // all other cells in this block cannot hold the number
    }
  }

  /**
   * Excludes cells based on the the content of a block. If the exact position of a particular number is unknown, but it
   * can only be placed in one row or column of a block,
   * this means the row or column in the related blocks cannot holds this number.
   */
  // Excludes cells based on the the content of a block. If the exact position of a particular number is unknown, but it
  // can only be placed in one row or column of a block,
  // this means the row or column in the related blocks cannot hold this number.
  def excludeCellsByBlockVsLineOrColumn(): Unit = {
    linePartsPerBlock.map(_.filter(!_.allZeroes())).foreach {
      // this block contains two lines where the number can't be placed. It must be place on line listPart.index
      // exclude all the other cells in the line outside this block
      case listPart :: Nil => for {cell <- lines(listPart.index) if mod(cell.x) != listPart.partIndex} cell.setValue(false)
      case _ => ()
    }
    columnPartsPerBlock.map(_.filter(!_.allZeroes())).foreach {
      // this block contains two lines where the number can't be placed. It must be place on column listPart.index
      // exclude all the other cells in the column outside this block
      case listPart :: Nil => for {cell <- columns(listPart.index) if mod(cell.y) != listPart.partIndex} cell.setValue(false)
      case _ => ()
    }
  }

  // Join all the lines, columns and block and search
  def findSolution(): List[ValueCell] =
    lines.values ++ columns.values ++ blocks.values flatMap findUniqueCandidates toList

  // find the lists of cells holding exactly one empty cell. This cell should contain the number
  def findUniqueCandidates(cells: List[BinaryCell]): Option[ValueCell] =
    cells.filter(_.value.isEmpty) match {
      case i :: Nil if !cells.exists(_.value.contains(true)) => Some(ValueCell(i.x, i.y, this.index))
      case _ => None
    }

  def printLayer(): Unit = {
    var y = 1
    while (y <= 9) {
      var x = 1
      if ((y - 1) % 3 == 0) println(" -------------------")
      while (x <= 9) {
        if ((x - 1) % 3 == 0) print(" | ")
        print(cells.find(_.hasCoordinates(x, y)).get.value match {
          case None => "-"
          case Some(true) => "1"
          case Some(false) => "0"
        })
        x = x + 1
      }
      println(" |")
      y = y + 1
    }
    println(" -------------------")
    println(this.index)

  }

  // Calculate block id for a cell
  def getBlockId(x: Int, y: Int): Int = mod(x) + ((y - 1) / 3) * 3

  // Helper
  def mod(i: Int): Int = 1 + ((i - 1) / 3)

}

object BooleanLayer {
  // use the cells with the same coords on all layers to see if only one of them is empty
  // If so, it is the sole candidate for that cell
  def findSoleCandidates(layers: List[BooleanLayer]): List[ValueCell] = {
    val cellsByCoordinates = for {
      x <- 1 to 9
      y <- 1 to 9
    } yield layers.map(layer => (layer.index, layer.cells.find(_.hasCoordinates(x, y)).get))

    cellsByCoordinates.flatMap(cellsForCoordinate => cellsForCoordinate.filter(_._2.value.isEmpty) match {
      case (value, cell) :: Nil => Some(ValueCell(cell.x, cell.y, value))
      case _ => None
    }) toList
  }
}

case class ListPart(index: Int, partIndex: Int, cells: List[BinaryCell]) {
  def allZeroes(): Boolean = cells.forall(_.value.contains(false))
}

case class Block(x: Int, y: Int, lines: List[ListPart], columns: List[ListPart])