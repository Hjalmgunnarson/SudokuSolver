package com.hjalmgunnarson

import org.scalatest.{FunSpec, Matchers}

import scala.collection._

class BooleanLayerSpec extends FunSpec with Matchers {

  describe("A BooleanLayer") {

    it("should exclude lines, columns and blocks when a value is placed in the layer that holds the number") {
      val layer = BooleanLayer(1)

      layer.setValue(1, 1, 1)
      assert(layer.cells.filter(c => c.y == 1 && c.x == 1).forall(_.value.contains(true)), "The cell must contain true")
      assert(layer.cells.filter(c => c.y == 1 && c.x != 1).forall(_.value.contains(false)),
        "All other cells in the line must contain false")
      assert(layer.cells.filter(c => c.y != 1 && c.x == 1).forall(_.value.contains(false)),
        "All other cells in the column must contain false")
      assert(layer.cells.filter(c => getBlockId(c.x, c.y) == 1 && c.y != 1 && c.x == 1).forall(_.value.contains(false)),
        "All other cells in the block must contain false")
    }

    it("should only exclude the cell when a value is placed in the layer that holds another number") {
      val layer = BooleanLayer(1)
      layer.setValue(1, 1, 2)
      assert(layer.cells.filter(c => c.y == 1 && c.x == 1).forall(_.value.contains(false)), "The cell must contain false")
      assert(layer.cells.filter(c => c.y != 1 && c.x != 1).forall(_.value.isEmpty), "All other cells must be empty")
    }

    it("should exclude the cells of the same line of two blocks if a value can only placed in one line of the third block") {
      val layer = BooleanLayer(1)
      // Set two rows of the first block to zeroes
      for {
        x <- 1 to 3
        y <- 1 to 2
      } layer.cells.find(cell => cell.x == x && cell.y == y).foreach(_.setValue(false))

      layer.excludeCellsByBlockVsLineOrColumn()
      layer.printLayer()
      assert(layer.cells.filter(c => c.x > 3 && c.y == 3).forall(_.value.contains(false)))
    }

    it("should exclude the cells of the same column of two blocks if a value can only placed in one column of the third block") {
      val layer = BooleanLayer(1)
      // Set two columns of the first block to zeroes
      for {
        x <- 1 to 2
        y <- 1 to 3
      } layer.cells.find(cell => cell.x == x && cell.y == y).foreach(_.setValue(false))

      layer.excludeCellsByBlockVsLineOrColumn()
      assert(layer.cells.filter(c => c.y > 3 && c.x == 3).forall(_.value.contains(false)))
    }

    it("should identify the only empty cell in a line, column or block of cells as a candidate") {
      val layer = BooleanLayer(1)
      for {
        cell <- layer.lines(1)
        if cell.x < 9
      } cell.setValue(false)

      assert(layer.findSolution().head == ValueCell(9, 1, 1))
    }

    it("should identify the only empty cell from a list of cells that have the same coordinates across all layers") {
      val layers: Seq[BooleanLayer] = for {
        index <- 1 to 9
      } yield BooleanLayer(index)

      for {
        layer <- layers
        cell <- layer.cells
        if layer.value > 1 && cell.x == 1 && cell.y == 1
      } cell.setValue(false)

      assert(BooleanLayer.findSoleCandidates(layers).head == ValueCell(1, 1, 1))
    }


    it("should exclude cells from the lines of a block when two related blocks have candidates only in those rows ") {
      val layer = BooleanLayer(1)
      // Set first row of the first two block to zeroes
      for {
        x <- 1 to 6
      } layer.cells.find(cell => cell.x == x && cell.y == 3).foreach(_.setValue(false))

      layer.excludeCellsByCombiningBlocks()
      layer.printLayer()
      assert(layer.cells.filter(c => c.x > 6 && (c.y == 1 || c.y == 2)).forall(_.value.contains(false)))
    }

    it("should exclude cells from the columns of a block when two related blocks have candidates only in those columns ") {
      val layer = BooleanLayer(1)
      // Set first row of the first two block to zeroes
      for {
        y <- 1 to 6
      } layer.cells.find(cell => cell.y == y && cell.x == 3).foreach(_.setValue(false))

      layer.excludeCellsByCombiningBlocks()
      layer.printLayer()
      assert(layer.cells.filter(c => c.y > 6 && (c.x == 1 || c.x == 2)).forall(_.value.contains(false)))
    }
  }

  def getBlockId(x: Int, y: Int): Int = mod(x) + ((y - 1) / 3) * 3

  def mod(i: Int): Int = 1 + ((i - 1) / 3)

}
