package org.sutemi.sudoku

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

/**
 * Created with IntelliJ IDEA.
 * User: chris
 * Date: 8/14/13
 * Time: 7:52 PM
 * To change this template use File | Settings | File Templates.
 */
@RunWith(classOf[JUnitRunner])
class SudokuGridSpecification extends FunSpec with ShouldMatchers {
  describe("A SudokuGrid") {

    it("should find cells in the row") {
      assert(SudokuGrid.getRowCells(1,3) === Vector((1,0),(1,1),(1,2),(1,4),(1,5),(1,6),(1,7),(1,8)))
    }

    it("should find cells in the column") {
      assert(SudokuGrid.getColCells(1,3) === Vector((0,3),(2,3),(3,3),(4,3),(5,3),(6,3),(7,3),(8,3)))
    }

    it("should find peer cells in the unit") {
      assert(SudokuGrid.getPeerCells(0,0) === Vector((0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)))
      assert(SudokuGrid.getPeerCells(1,3) === Vector((0,3),(0,4),(0,5),(1,4),(1,5),(2,3),(2,4),(2,5)))
      assert(SudokuGrid.getPeerCells(7,8) === Vector((6,6),(6,7),(6,8),(7,6),(7,7),(8,6),(8,7),(8,8)))
    }

    it("should remove a single possibility") {
      val grid = SudokuGrid()
      val newgrid = grid.removePossibility(3,4,5)
      newgrid should not be (ContradictorySudokuGrid)
      assert(newgrid.countPossibilities(3,4) === 8)
    }

    it("should remove many possibilities") {
      val empty = SudokuGrid()
      val removed = (1 to 8).foldLeft(empty.asInstanceOf[SudokuGrid])((grid,poss) => grid.removePossibility(0,0,poss))
      assert(removed.countPossibilities(0,0) === 1)
    }

    it("should return none when removing only possibility") {
      val empty = SudokuGrid()
      val prepped = (1 to 8).foldLeft(empty.asInstanceOf[SudokuGrid])((grid,poss) => grid.removePossibility(0,0,poss))
      val removed = prepped.removePossibility(0,0,9)
      removed should be (ContradictorySudokuGrid)
    }

    it("should place a conjecture") {
      val empty = SudokuGrid()
      val placed = empty.placeConjecture(0,0,5)
      placed should not be (ContradictorySudokuGrid)
      assert(placed.countPossibilities(0,0) === 1)
    }

    it("should place a conjecture not in 0,0") {
      val empty = SudokuGrid()
      val placed = empty.placeConjecture(1,1,8)
      placed should not be (ContradictorySudokuGrid)
      assert(placed.countPossibilities(1,1) === 1)
      assert(placed.countPossibilities(1,0) === 8)
    }

    it("should propagate a conjecture") {
      val empty = SudokuGrid()
      val placed = empty.placeConjecture(0,0,5)
      for (i <- 1 until 9) {
        assert(placed.countPossibilities(i,0) === 8)
        assert(placed.countPossibilities(0,i) === 8)
      }
    }

    it("should return contradiction when conjecturecountPossibilities is not possible") {
      val empty = SudokuGrid()
      val removed = empty.removePossibility(0,0,5)
      val placed = removed.placeConjecture(0,0,5)
      placed should be (ContradictorySudokuGrid)
    }

    it("should place conjecture when all possibilities removed") {
      val empty = SudokuGrid()
      val prepped = (1 to 8).foldLeft(empty.asInstanceOf[SudokuGrid])((grid,poss) => grid.removePossibility(0,0,poss))
      assert(prepped.countPossibilities(0,0) === 1)
      for (i <- 1 until 9) {
        assert(prepped.countPossibilities(0,i) === 8)
        assert(prepped.countPossibilities(i,0) === 8)
      }
    }

    it("should handle propagating contradictions") {
      val contradiction = ContradictorySudokuGrid
      val placed = contradiction.placeConjecture(0,0,5)
      val removed = contradiction.removePossibility(0,0,5)
      placed should be (ContradictorySudokuGrid)
      removed should be (ContradictorySudokuGrid)
      assert(contradiction.countPossibilities(0,0) === 0)
    }

    it("should return a contradiction when propagation leads to conflict") {
      val empty = SudokuGrid()
      val prepped = List(1,3,4,5,6,7,9).foldLeft(empty.asInstanceOf[SudokuGrid])((grid,poss) =>
        grid.removePossibility(0,2,poss).removePossibility(0,3,poss).removePossibility(0,4,poss))
      val placed = prepped.placeConjecture(0,2,2)
      placed should be (ContradictorySudokuGrid)
    }

    it("should populate a grid from a list of givens") {
      val empty = SudokuGrid()
      val givens = empty.placeConjectures(List((0,0,5),(1,1,8)))
      assert(givens.countPossibilities(0,0) === 1)
      assert(givens.countPossibilities(0,3) === 8)
      assert(givens.countPossibilities(0,1) === 7)
      assert(givens.countPossibilities(1,1) === 1)
    }

    it("should populate a grid from a string") {
      val givens = SudokuGrid("500000000080000000000000000000000000000000000000000000000000000000000000000000000")
      assert(givens.isInstanceOf[LiveSudokuGrid])
      assert(givens.countPossibilities(0,0) === 1)
      assert(givens.countPossibilities(0,3) === 8)
      assert(givens.countPossibilities(0,1) === 7)
      assert(givens.countPossibilities(1,1) === 1)
    }


    it("should identify a solution") {
      val empty = SudokuGrid()
      val contradiction = ContradictorySudokuGrid
      val points = List((0,0,1),(0,1,4),(0,2,7),(0,3,2),(0,4,5),(0,5,8),(0,6,3),(0,7,6),(0,8,9),
      (1,0,2),(1,1,5),(1,2,8),(1,3,3),(1,4,6),(1,5,9),(1,6,1),(1,7,4),(1,8,7),
      (2,0,3),(2,1,6),(2,2,9),(2,3,1),(2,4,4),(2,5,7),(2,6,2),(2,7,5),(2,8,8),
      (3,0,4),(3,1,7),(3,2,1),(3,3,8),(3,4,2),(3,5,5),(3,6,9),(3,7,3),(3,8,6),
      (4,0,5),(4,1,8),(4,2,2),(4,3,9),(4,4,3),(4,5,6),(4,6,7),(4,7,1),(4,8,4),
      (5,0,6),(5,1,9),(5,2,3),(5,3,7),(5,4,1),(5,5,4),(5,6,8),(5,7,2),(5,8,5),
      (6,0,7),(6,1,1),(6,2,4),(6,3,5),(6,4,8),(6,5,2),(6,6,6),(6,7,9),(6,8,3),
      (7,0,8),(7,1,2),(7,2,5),(7,3,6),(7,4,9),(7,5,3),(7,6,4),(7,7,7),(7,8,1),
      (8,0,9),(8,1,3),(8,2,6),(8,3,4),(8,4,7),(8,5,1),(8,6,5),(8,7,8),(8,8,2))
      val prepped = empty.placeConjectures(points)
      assert(empty.isSolution === false)
      assert(contradiction.isSolution === false)
      assert(prepped.isSolution === true)
    }

    it("should identify the first cell with minimal possibilities") {
      val empty = SudokuGrid()
      assert(empty.minimalPossibilityCell === Some(0,0))
      val givens = SudokuGrid("500000000080000000000000000000000000000000000000000000000000000000000000000000000")
      assert(givens.minimalPossibilityCell === Some((0,1)))
      val removed = givens.removePossibility(5,5,1).removePossibility(5,5,2).removePossibility(5,5,3).removePossibility(5,5,4)
      assert(removed.minimalPossibilityCell === Some((5,5)))
      val solved = SudokuGrid("147258369258369147369147258471825936582936714693714825714582693825693471936471582")
      assert(solved.minimalPossibilityCell === None)
    }

    it("should generate a printable string for an empty grid") {
      val empty = SudokuGrid()
      val expectedstring =
        "[. . . | . . . | . . .\n" +
          ". . . | . . . | . . .\n" +
          ". . . | . . . | . . .\n" +
          "---------------------\n" +
          ". . . | . . . | . . .\n" +
          ". . . | . . . | . . .\n" +
          ". . . | . . . | . . .\n" +
          "---------------------\n" +
          ". . . | . . . | . . .\n" +
          ". . . | . . . | . . .\n" +
          ". . . | . . . | . . .]"
      assert(empty.toString() === expectedstring)
    }

    it("should generate a printable string for a partially filled grid") {
      val empty = SudokuGrid()
      val placed = empty.placeConjectures(List((0,0,1),(3,4,5)))
      val expectedstring =
        "[1 . . | . . . | . . .\n" +
          ". . . | . . . | . . .\n" +
          ". . . | . . . | . . .\n" +
          "---------------------\n" +
          ". . . | . 5 . | . . .\n" +
          ". . . | . . . | . . .\n" +
          ". . . | . . . | . . .\n" +
          "---------------------\n" +
          ". . . | . . . | . . .\n" +
          ". . . | . . . | . . .\n" +
          ". . . | . . . | . . .]"
      assert(placed.toString() === expectedstring)
    }
  }

  describe("a sudoku solver") {
    it ("should do something reasonable for a contradictory puzzle") {
      assert(SudokuGrid.solve(ContradictorySudokuGrid) === None)
    }

    it ("should do something reasonable for an empty puzzle") {
      assert(SudokuGrid.solve(SudokuGrid()) === None)
    }

    ignore ("should solve an already solved puzzle") {
      val solvedGiven = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
      val solvedGrid = SudokuGrid.solve(SudokuGrid(solvedGiven))
      assert(solvedGrid.isInstanceOf[Some[SudokuGrid]])
    }

    ignore ("should solve a search puzzle") {
      val unsolvedGiven = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
      val solvedGrid = SudokuGrid.solve(SudokuGrid(unsolvedGiven))
      assert(solvedGrid.isInstanceOf[Some[SudokuGrid]])
    }
  }
}
