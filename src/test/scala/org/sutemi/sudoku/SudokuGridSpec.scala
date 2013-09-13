package org.sutemi.sudoku

import org.specs2.mutable._

class SudokuGridSpec extends Specification {

  "A SudokuGrid" should {
    "find cells in the row" in {
      SudokuGrid.getRowCells(1,3) must beEqualTo(Vector((1,0),(1,1),(1,2),(1,4),(1,5),(1,6),(1,7),(1,8)))
    }

    "find cells in the column" in {
      SudokuGrid.getColCells(1,3) must beEqualTo(Vector((0,3),(2,3),(3,3),(4,3),(5,3),(6,3),(7,3),(8,3)))
    }

    "find peer cells in the unit" in {
      SudokuGrid.getPeerCells(0,0) must beEqualTo(Vector((0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)))
      SudokuGrid.getPeerCells(1,3) must beEqualTo(Vector((0,3),(0,4),(0,5),(1,4),(1,5),(2,3),(2,4),(2,5)))
      SudokuGrid.getPeerCells(7,8) must beEqualTo(Vector((6,6),(6,7),(6,8),(7,6),(7,7),(8,6),(8,7),(8,8)))
    }
  }
}
