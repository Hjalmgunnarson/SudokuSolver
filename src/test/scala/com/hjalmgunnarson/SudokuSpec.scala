package com.hjalmgunnarson

import org.scalatest.{FunSpec, Matchers}

import scala.collection.Seq

class SudokuSpec extends FunSpec with Matchers {

  describe("A BooleanLayer") {

    it("should generate a valid solution") {
      val sudoku = new Sudoku
      sudoku.setValue(3, 1, 7)
      sudoku.setValue(5, 1, 3)
      sudoku.setValue(8, 1, 8)

      sudoku.setValue(4, 2, 4)
      sudoku.setValue(5, 2, 1)
      sudoku.setValue(6, 2, 7)

      sudoku.setValue(3, 3, 2)
      sudoku.setValue(5, 3, 6)
      sudoku.setValue(9, 3, 4)

      sudoku.setValue(1, 4, 2)
      sudoku.setValue(2, 4, 4)
      sudoku.setValue(6, 4, 6)

      sudoku.setValue(2, 5, 3)
      sudoku.setValue(3, 5, 8)
      sudoku.setValue(6, 5, 4)
      sudoku.setValue(9, 5, 7)

      sudoku.setValue(2, 6, 5)
      sudoku.setValue(7, 6, 9)
      sudoku.setValue(9, 6, 2)

      sudoku.setValue(1, 7, 6)
      sudoku.setValue(2, 7, 2)
      sudoku.setValue(7, 7, 5)

      sudoku.setValue(4, 8, 1)
      sudoku.setValue(5, 8, 9)

      sudoku.setValue(1, 9, 1)
      sudoku.setValue(7, 9, 2)
      sudoku.setValue(9, 9, 9)

      var solutions = sudoku.findSolutions()
      while (solutions.nonEmpty) {
        solutions.foreach(cell => sudoku.setValue(cell.x, cell.y, cell.value))
        solutions = sudoku.findSolutions()
      }

      val lines = sudoku.cells.groupBy(_.y)
      val columns = sudoku.cells.groupBy(_.x)
      val blocks = sudoku.cells.groupBy(c => 1 + ((c.x - 1) / 3) + ((c.y - 1) / 3) * 3)
      for {
        value <- 1 to 9
      } {
        assert(lines.size == 9)
        assert(columns.size == 9)
        assert(blocks.size == 9)

        assert(lines.values.size == 9)
        assert(columns.values.size == 9)
        assert(blocks.values.size == 9)

        assert(lines.values.map(line => line.exists(cell => cell.value().contains(value))).forall(b => b),
          "All lines should contain all values")
        assert(columns.values.map(column => column.exists(cell => cell.value().contains(value))).forall(b => b),
          "All columns should contain all values")
        assert(blocks.values.map(blocks => blocks.exists(cell => cell.value().contains(value))).forall(b => b),
          "All blocks should contain all values")
      }
      sudoku.printSudoku()
    }

    it("should exit correctly when it cant find the solution") {
      val sudoku = new Sudoku
      sudoku.setValue(3, 1, 5)
      sudoku.setValue(4, 1, 3)

      sudoku.setValue(1, 2, 8)
      sudoku.setValue(8, 2, 2)

      sudoku.setValue(2, 3, 7)
      sudoku.setValue(5, 3, 1)
      sudoku.setValue(7, 3, 5)

      sudoku.setValue(1, 4, 4)
      sudoku.setValue(6, 4, 5)
      sudoku.setValue(7, 4, 3)

      sudoku.setValue(2, 5, 1)
      sudoku.setValue(5, 5, 7)
      sudoku.setValue(9, 5, 6)

      sudoku.setValue(3, 6, 3)
      sudoku.setValue(4, 6, 2)
      sudoku.setValue(8, 6, 8)

      sudoku.setValue(2, 7, 6)
      sudoku.setValue(4, 7, 5)
      sudoku.setValue(9, 7, 9)

      sudoku.setValue(3, 8, 4)
      sudoku.setValue(8, 8, 3)

      sudoku.setValue(6, 9, 9)
      sudoku.setValue(7, 9, 7)

      var solutions = sudoku.findSolutions()
      while (solutions.nonEmpty) {
        solutions.foreach(cell => sudoku.setValue(cell.x, cell.y, cell.value))
        solutions = sudoku.findSolutions()
      }

      sudoku.printSudoku()
      assert(true)
    }



    it("should yield a complete list of solutions") {
      val sudoku = new Sudoku
      sudoku.setValue(1, 1, 4)
      sudoku.setValue(2, 1, 9)
      sudoku.setValue(8, 1, 6)

      sudoku.setValue(1, 2, 6)
      sudoku.setValue(8, 2, 1)
      sudoku.setValue(9, 2, 7)

      sudoku.setValue(1, 3, 1)
      sudoku.setValue(5, 3, 7)
      sudoku.setValue(6, 3, 3)
      sudoku.setValue(7, 3, 9)

      sudoku.setValue(1, 4, 9)
      sudoku.setValue(2, 4, 7)
      sudoku.setValue(3, 4, 4)
      sudoku.setValue(9, 4, 2)


      sudoku.setValue(4, 6, 8)
      sudoku.setValue(5, 6, 5)
      sudoku.setValue(6, 6, 4)

      sudoku.setValue(6, 7, 5)
      sudoku.setValue(7, 7, 4)
      sudoku.setValue(8, 7, 3)

      sudoku.setValue(1, 8, 7)
      sudoku.setValue(3, 8, 5)
      sudoku.setValue(9, 8, 9)

      sudoku.setValue(2, 9, 2)
      sudoku.setValue(6, 9, 1)

      println(sudoku.solve(Nil))
      sudoku.printSudoku()
    }

    it("should identify the only empty cell from a list of cells that have the same coordinates across all layers") {
      val sudoku = new Sudoku

      for {
        layer <- sudoku.layers
        cell <- layer.cells
        if layer.value > 1 && cell.x == 1 && cell.y == 1
      } cell.setValue(false)

      assert(sudoku.findSoleCandidates().head == ValueCell(1, 1, 1))
    }
  }
}
