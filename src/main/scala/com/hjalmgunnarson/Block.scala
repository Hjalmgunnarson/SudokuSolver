package com.hjalmgunnarson

import scala.language.postfixOps

/**
 * Represents a block of three by three cells. A Sudoku puzzle consists of three of these blocks
 *
 * @param x     the x coordinate of the block
 * @param y     the y coordinate of the block
 * @param cells the nine cells of this block
 */
case class Block(x: Int, y: Int, cells: Seq[BooleanCell]) {

  // Helper class
  case class ListPart(index: Int, cells: Seq[BooleanCell])

  val lines: Seq[ListPart] = cells.groupBy(_.y).map { case (y, list) => ListPart(y, list) } toSeq
  val columns: Seq[ListPart] = cells.groupBy(_.x).map { case (x, list) => ListPart(x, list) } toSeq

  def getCandidateLine: Option[Int] = getCandidateListPart(lines, someCellsEmpty)

  def getCandidateColumn: Option[Int] = getCandidateListPart(columns, someCellsEmpty)

  def getUnavailableLine: Option[Int] = getCandidateListPart(lines, allCellsFalse)

  def getUnavailableColumn: Option[Int] = getCandidateListPart(columns, allCellsFalse)

  def hasOneUnavailableLine: Boolean = getUnavailableLine.nonEmpty

  def hasOneUnavailableColumn: Boolean = getUnavailableColumn.nonEmpty

  def containsCell(x: Int, y: Int): Boolean = cells.exists(c => c.x == x && c.y == y)

  // Finds exactly one listPart that satisfies the predicate and optionally returns its index
  private def getCandidateListPart(lists: Seq[ListPart], predicate: ListPart => Boolean): Option[Int] = {
    lists.filter(listPart => predicate(listPart)) match {
      case Seq(listPart) => Some(listPart.index)
      case _ => None
    }
  }


  def allCellsFalse(listPart: ListPart): Boolean = listPart.cells.forall(_.value.contains(false))

  def someCellsEmpty(listPart: ListPart): Boolean = listPart.cells.exists(_.value.isEmpty)


}
