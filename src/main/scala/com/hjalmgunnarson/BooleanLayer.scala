package com.hjalmgunnarson

import scala.language.postfixOps

case class BooleanLayer(value: Int, cells: Seq[BinaryCell]) {
  // Lines contains the horizontal lists of cells.
  val lines: Map[Int, Seq[BinaryCell]] = 1 to 9 map (i => (i, cells.filter(_.y == i))) toMap
  // Columns contains the vertical lists of cells.
  val columns: Map[Int, Seq[BinaryCell]] = 1 to 9 map (i => (i, cells.filter(_.x == i))) toMap
  // Blocks contains the lists of blocks that hold a list of cells contained by the block
  val blocks: Seq[Block] = for {
    x <- 1 to 3
    y <- 1 to 3
  } yield Block(x, y, cells.filter(cell => mod(cell.x) == x && mod(cell.y) == y))

  // Sets value in designated cell and excludes cells with the same coordinates on the other layers
  // Excludes cells in the same line, column and block on this layer
  def setValue(x: Int, y: Int, value: Int): Unit = {
    cells.find(_.hasCoordinates(x, y)).foreach(_.setValue(this.value == value))
    if (this.value == value) {
      for {cell <- lines(y) if cell.x != x} cell.setValue(false) // all other cells on this line cannot hold the number
      for {cell <- columns(x) if cell.y != y} cell.setValue(false) // all other cells in this column cannot hold the number
      for {
        block <- blocks
        if block.containsCell(x, y)
        cell <- block.cells
        if cell.x != x && cell.y != y
      } cell.setValue(false) // all other cells in this block cannot hold the number
    }
  }

  def excludeCells(): Unit = {
    excludeCellsByBlockVsLineOrColumn()
    excludeCellsByCombiningBlocks()
  }

  /**
   * Excludes cells based on the content of a block. If the exact position of a particular number is unknown, but it
   * can only be placed in one row or column of a block,
   * this means the row or column in the related blocks cannot holds this number.
   */
  def excludeCellsByBlockVsLineOrColumn(): Unit = {
    for {
      (_, groupedBlocks) <- blocks.groupBy(_.y) // For each horizontal row of three blocks
      blockUnderInspection <- groupedBlocks // Check each block
      y <- blockUnderInspection.getCandidateLine // And see if it has only one line that can hold the value
      blockToBeExcluded <- groupedBlocks
      if blockToBeExcluded.x != blockUnderInspection.x
      cell <- blockToBeExcluded.cells
      if cell.y == y
      if cell.value.isEmpty
    } cell.setValue(false)

    for {
      (_, groupedBlocks) <- blocks.groupBy(_.x) // For each vertical row of three blocks
      blockUnderInspection <- groupedBlocks // Check each block
      x <- blockUnderInspection.getCandidateColumn // And see if it has only one line that can hold the value
      blockToBeExcluded <- groupedBlocks
      if blockToBeExcluded.y != blockUnderInspection.y
      cell <- blockToBeExcluded.cells
      if cell.x == x
      if cell.value.isEmpty
    } cell.setValue(false)
  }

  /**
   * Exclude cells based on the content of two blocks. If two cells both have the same row or column in which the value
   * cannot be placed, then it must be placed in the row or column of the third block. Thus the other rows or columns of
   * the third block can be excluded.
   *
   */
  def excludeCellsByCombiningBlocks(): Unit = {
    for {
      (_, groupedBlocks) <- blocks.groupBy(_.y)
    } groupedBlocks.filter(_.hasOneUnavailableLine) match {
      case Seq(block1, block2) if block1.getUnavailableLine == block2.getUnavailableLine =>
        for {
          block <- groupedBlocks
          if block != block1 && block != block2
          y <- block1.getUnavailableLine
          cell <- block.cells
          if cell.y != y
          if cell.value.isEmpty
        } cell.setValue(false)
      case _ => ()
    }

    for {
      (_, groupedBlocks) <- blocks.groupBy(_.x)
    } groupedBlocks.filter(_.hasOneUnavailableColumn) match {
      case Seq(block1, block2) if block1.getUnavailableColumn == block2.getUnavailableColumn =>
        for {
          block <- groupedBlocks
          if block != block1 && block != block2
          x <- block1.getUnavailableColumn
          cell <- block.cells
          if cell.x != x
          if cell.value.isEmpty
        } cell.setValue(false)
      case _ => ()
    }
  }

  // Join all the lines, columns and block and search
  def findSolution(): Seq[ValueCell] = {
    val allLists = lines.values ++ columns.values ++ blocks.map(_.cells)
    allLists.flatMap(list => findUniqueCandidate(list, this.value)) toSeq
  }

  // Check if the list contains exactly one empty cell. When there are no cells holding true, this cell should contain the number
  def findUniqueCandidate(cells: Seq[BinaryCell], value: Int): Option[ValueCell] =
    cells.filter(_.value.isEmpty) match {
      case Seq(cell) if !cells.exists(_.value.contains(true)) => Some(ValueCell(cell.x, cell.y, value))
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
    println(this.value)

  }

  // Calculate block id for a cell
  def getBlockId(x: Int, y: Int): Int = mod(x) + ((y - 1) / 3) * 3

  // Helper
  def mod(i: Int): Int = 1 + ((i - 1) / 3)

}

object BooleanLayer {
  // use the cells with the same coords on all layers to see if only one of them is empty
  // If so, it is the sole candidate for that cell
  def findSoleCandidates(layers: Seq[BooleanLayer]): Seq[ValueCell] = {
    val cellsByCoordinates = for {
      x <- 1 to 9
      y <- 1 to 9
    } yield layers.map(layer => (layer.value, layer.cells.find(_.hasCoordinates(x, y)).get))
    // TODO: Remove.get above
    cellsByCoordinates.flatMap(cellsForCoordinate => cellsForCoordinate.filter(_._2.value.isEmpty) match {
      case Seq((value, cell)) => Some(ValueCell(cell.x, cell.y, value))
      case _ => None
    }) toList
  }
}

