package org.sutemi.sudoku

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner

/**
 * Created with IntelliJ IDEA.
 * User: chris
 * Date: 8/14/13
 * Time: 7:52 PM
 * To change this template use File | Settings | File Templates.
 */
@RunWith(classOf[JUnitRunner])
class SudokuGridSpecification extends FunSpec {
  describe("A SudokuGrid") {

    it("should find cells in the row") {
      val grid = new SudokuGrid
      assert(grid.getRowCells(1,3) === Vector((1,0),(1,1),(1,2),(1,4),(1,5),(1,6),(1,7),(1,8)))
    }

    it("should find cells in the column") {
      val grid = new SudokuGrid
      assert(grid.getColCells(1,3) === Vector((0,3),(2,3),(3,3),(4,3),(5,3),(6,3),(7,3),(8,3)))
    }

    it("should find peer cells in the unit") {
      val grid = new SudokuGrid
      assert(grid.getPeerCells(0,0) === Vector((0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)))
      assert(grid.getPeerCells(1,3) === Vector((0,3),(0,4),(0,5),(1,4),(1,5),(2,3),(2,4),(2,5)))
      assert(grid.getPeerCells(7,8) === Vector((6,6),(6,7),(6,8),(7,6),(7,7),(8,6),(8,7),(8,8)))
    }

    it("should remove a single possibility") {
      val grid = new SudokuGrid
      val newgrid = grid.removePossibility(3,4,5)
      assert(newgrid.countPossibilities(3,4) === 8)
    }

    it("should remove many possibilities") {
      val empty = new SudokuGrid
      val removedPoss = empty.removePossibility(0,0,1)
        .removePossibility(0,0,2)
        .removePossibility(0,0,3)
        .removePossibility(0,0,4)
        .removePossibility(0,0,5)
        .removePossibility(0,0,6)
        .removePossibility(0,0,7)
        .removePossibility(0,0,8)
      assert(removedPoss.countPossibilities(0,0) === 1)
    }

  }
}
