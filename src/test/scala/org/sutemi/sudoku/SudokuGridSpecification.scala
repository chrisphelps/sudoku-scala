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
  }
}
