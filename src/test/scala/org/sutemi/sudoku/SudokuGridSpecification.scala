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
      val grid = new LiveSudokuGrid
      val newgrid = grid.removePossibility(3,4,5)
      newgrid should not be (ContradictorySudokuGrid)
      assert(newgrid.countPossibilities(3,4) === 8)
    }

    it("should remove many possibilities") {
      val empty = new LiveSudokuGrid
      val removedPoss =
            empty.removePossibility(0,0,1)
            .removePossibility(0,0,2)
            .removePossibility(0,0,3)
            .removePossibility(0,0,4)
            .removePossibility(0,0,5)
            .removePossibility(0,0,6)
            .removePossibility(0,0,7)
            .removePossibility(0,0,8)
      assert(removedPoss.countPossibilities(0,0) === 1)
    }

    it("should return none when removing only possibility") {
      val empty = new LiveSudokuGrid
      val prepped =
            empty.removePossibility(0,0,1)
            .removePossibility(0,0,2)
            .removePossibility(0,0,3)
            .removePossibility(0,0,4)
            .removePossibility(0,0,5)
            .removePossibility(0,0,6)
            .removePossibility(0,0,7)
            .removePossibility(0,0,8)
      val removed = prepped.removePossibility(0,0,9)
      removed should be (ContradictorySudokuGrid)
    }

    it("should place a conjecture") {
      val empty = new LiveSudokuGrid
      val placed = empty.placeConjecture(0,0,5)
      placed should not be (ContradictorySudokuGrid)
      assert(placed.countPossibilities(0,0) === 1)
    }

    it("should place a conjecture not in 0,0") {
      val empty = new LiveSudokuGrid
      val placed = empty.placeConjecture(1,1,8)
      placed should not be (ContradictorySudokuGrid)
      assert(placed.countPossibilities(1,1) === 1)
      assert(placed.countPossibilities(1,0) === 8)
    }

    it("should propagate a conjecture") {
      val empty = new LiveSudokuGrid
      val placed = empty.placeConjecture(0,0,5)
      for (i <- 1 until 9) {
        assert(placed.countPossibilities(i,0) === 8)
        assert(placed.countPossibilities(0,i) === 8)
      }
    }

    it("should return none when conjecture is not possible") {
      val empty = new LiveSudokuGrid
      val removed = empty.removePossibility(0,0,5)
      val placed = removed.placeConjecture(0,0,5)
      placed should be (ContradictorySudokuGrid)
    }

    it("should place conjecture when all possibilities removed") {
      val empty = new LiveSudokuGrid
      val prepped =
          empty.removePossibility(0,0,1)
          .removePossibility(0,0,2)
          .removePossibility(0,0,3)
          .removePossibility(0,0,4)
          .removePossibility(0,0,5)
          .removePossibility(0,0,6)
          .removePossibility(0,0,7)
          .removePossibility(0,0,8)
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

    it("should populate a grid from a list of givens") {
      val empty = new LiveSudokuGrid
      val givens = empty.placeConjectures(List((0,0,5),(1,1,8)))
      assert(givens.countPossibilities(0,0) === 1)
      assert(givens.countPossibilities(0,3) === 8)
      assert(givens.countPossibilities(0,1) === 7)
      assert(givens.countPossibilities(1,1) === 1)
    }

    // todo: test which does propagation and fails due to contradiction
  }
}
