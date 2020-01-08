package com.hjalmgunnarson

import org.scalatest.{FunSpec, Matchers}

import scala.collection._

class BooleanLayerSpec extends FunSpec with Matchers {

  describe("A BooleanLayer") {

    it("should exclude lines, columns and blocks when a value is placed in the layer that holds the number") {
      val cells = for {
        x <- 1 to 9
        y <- 1 to 9
      } yield BinaryCell(x, y, None)
      val layer = BooleanLayer(1, cells)

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
      val cells = for {
        x <- 1 to 9
        y <- 1 to 9
      } yield BinaryCell(x, y, None)
      val layer = BooleanLayer(1, cells)
      layer.setValue(1, 1, 2)
      assert(layer.cells.filter(c => c.y == 1 && c.x == 1).forall(_.value.contains(false)), "The cell must contain false")
      assert(layer.cells.filter(c => c.y != 1 && c.x != 1).forall(_.value.isEmpty), "All other cells must be empty")
    }

    it("should exclude the cells of the same line of two blocks if a value can only placed in one line of the third block") {
      val cells = for {
        x <- 1 to 9
        y <- 1 to 9
      } yield BinaryCell(x, y, None)
      val layer = BooleanLayer(1, cells)
      // Set two rows of the first block to zeroes
      layer.cells.filter(c => c.x > 0 && c.x < 4 && c.y > 0 && c.y < 3).foreach(_.setValue(false))

      layer.excludeCellsByBlockVsLineOrColumn()

      assert(layer.cells.filter(c => c.x > 3 && c.y == 3).forall(_.value.contains(false)))
    }

    it("should exclude the cells of the same column of two blocks if a value can only placed in one column of the third block") {
      val cells = for {
        x <- 1 to 9
        y <- 1 to 9
      } yield BinaryCell(x, y, None)
      val layer = BooleanLayer(1, cells)
      // Set two rows of the first block to zeroes
      layer.cells.filter(c => c.x > 0 && c.x < 3 && c.y > 0 && c.y < 4).foreach(_.setValue(false))

      layer.excludeCellsByBlockVsLineOrColumn()

      assert(layer.cells.filter(c => c.y > 3 && c.x == 3).forall(_.value.contains(false)))
    }

    it("should identify the only empty cell in a line, column or block of cells as a candidate") {
      val cells = for {
        x <- 1 to 8
      } yield BinaryCell(x, 1, Some(false))
      val layer = BooleanLayer(1, cells :+ BinaryCell(9, 1, None))

      assert(layer.findSolution().head == ValueCell(9, 1, 1))
    }

    it("should not identify the only empty cell in a line, column or block of cells as a candidate when it holds a " +
      "value in another cell") {
      val cells = for {
        x <- 1 to 8
      } yield BinaryCell(x, 1, Some(false))
      val layer = BooleanLayer(1, cells :+ BinaryCell(8, 1, None) :+ BinaryCell(8, 2, Some(true)) :+ BinaryCell(9,1, Some(true)))

      assert(layer.findSolution().headOption.isEmpty)
    }

    it("should identify the only empty cell from a list of cells that have the same coordinates across all layers") {
      val layers: Seq[BooleanLayer] = for {
        index <- 1 to 9
      } yield {
        val cells = for {
          x <- 1 to 9
          y <- 1 to 9
        } yield BinaryCell(x, y, None)
        BooleanLayer(index, cells)
      }

      for {
        layer <- layers
        cell <- layer.cells
        if layer.value > 1 && cell.x == 1 && cell.y == 1
      } cell.setValue(false)

      assert(BooleanLayer.findSoleCandidates(layers).head == ValueCell(1, 1, 1))
    }
  }

  def getBlockId(x: Int, y: Int): Int = mod(x) + ((y - 1) / 3) * 3

  def mod(i: Int): Int = 1 + ((i - 1) / 3)

}
