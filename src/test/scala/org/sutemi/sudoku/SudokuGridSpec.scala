package org.sutemi.sudoku

import org.specs2.mutable._
import org.sutemi.sudoku._

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

    "remove a single possibility" in {
      val grid = SudokuGrid()
      val newgrid = grid.removePossibility(3,4,5)
      newgrid must not be equalTo(ContradictorySudokuGrid)
      newgrid.countPossibilities(3,4) must beEqualTo(8)
    }

    "remove many possibilities" in {
      val empty = SudokuGrid()
      val removed = (1 to 8).foldLeft(empty.asInstanceOf[SudokuGrid])((grid,poss) => grid.removePossibility(0,0,poss))
      removed.countPossibilities(0,0) must beEqualTo(1)
    }

    "return contradiction when removing only possibility" in {
      val empty = SudokuGrid()
      val prepped = (1 to 8).foldLeft(empty.asInstanceOf[SudokuGrid])((grid,poss) => grid.removePossibility(0,0,poss))
      val removed = prepped.removePossibility(0,0,9)
      removed must beEqualTo(ContradictorySudokuGrid)
    }

    "place a conjecture" in {
      val empty = SudokuGrid()
      val placed = empty.placeConjecture(0,0,5)
      placed must not be equalTo(ContradictorySudokuGrid)
      placed.countPossibilities(0,0) must beEqualTo(1)
    }

    "place a conjecture not in 0,0" in {
      val empty = SudokuGrid()
      val placed = empty.placeConjecture(1,1,8)
      placed must not be equalTo(ContradictorySudokuGrid)
      placed.countPossibilities(1,1) must beEqualTo(1)
      placed.countPossibilities(1,0) must beEqualTo(8)
    }

    "propagate a conjecture" in {
      val empty = SudokuGrid()
      val placed = empty.placeConjecture(0,0,5)
      for (i <- 1 until 9) { // is this collecting the results correctly?
        placed.countPossibilities(i,0) must beEqualTo(8)
        placed.countPossibilities(0,i) must beEqualTo(8)
      }
      placed must not be equalTo(ContradictorySudokuGrid) // needed to have a return value?
    }

    "return contradiction when conjecture is not possible" in {
      val empty = SudokuGrid()
      val removed = empty.removePossibility(0,0,5)
      val placed = removed.placeConjecture(0,0,5)
      placed must beEqualTo(ContradictorySudokuGrid)
    }

    "place conjecture when all possibilities removed" in {
      val empty = SudokuGrid()
      val prepped = (1 to 8).foldLeft(empty.asInstanceOf[SudokuGrid])((grid,poss) => grid.removePossibility(0,0,poss))
      for (i <- 1 until 9) {
        prepped.countPossibilities(0,i) must beEqualTo(8)
        prepped.countPossibilities(i,0) must beEqualTo(8)
      }
      prepped.countPossibilities(0,0) must beEqualTo(1)
    }

    "handle propagating contradictions" in {
      val contradiction = ContradictorySudokuGrid
      val placed = contradiction.placeConjecture(0,0,5)
      val removed = contradiction.removePossibility(0,0,5)
      placed must beEqualTo(ContradictorySudokuGrid)
      removed must beEqualTo(ContradictorySudokuGrid)
      contradiction.countPossibilities(0,0) must beEqualTo(0)
      contradiction.placeConjectures(List((0,0,5),(0,0,7))) must beEqualTo(ContradictorySudokuGrid)
    }

    "return a contradiction when propagation leads to conflict" in {
      val empty = SudokuGrid()
      val prepped = List(1,3,4,5,6,7,9).foldLeft(empty.asInstanceOf[SudokuGrid])((grid,poss) =>
        grid.removePossibility(0,2,poss).removePossibility(0,3,poss).removePossibility(0,4,poss))
      val placed = prepped.placeConjecture(0,2,2)
      placed must beEqualTo(ContradictorySudokuGrid)
    }

    "populate a grid from a list of givens" in {
      val empty = SudokuGrid()
      val givens = empty.placeConjectures(List((0,0,5),(1,1,8)))
      givens.countPossibilities(0,0) must beEqualTo(1)
      givens.countPossibilities(0,3) must beEqualTo(8)
      givens.countPossibilities(0,1) must beEqualTo(7)
      givens.countPossibilities(1,1) must beEqualTo(1)
    }

    "populate a grid from a string" in {
      val givens = SudokuGrid("500000000080000000000000000000000000000000000000000000000000000000000000000000000")
      val unsolvedgivens = SudokuGrid("4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")
      givens must haveClass[LiveSudokuGrid]
      unsolvedgivens must haveClass[LiveSudokuGrid]
      givens.countPossibilities(0,0) must beEqualTo(1)
      givens.countPossibilities(0,3) must beEqualTo(8)
      givens.countPossibilities(0,1) must beEqualTo(7)
      givens.countPossibilities(1,1) must beEqualTo(1)
    }


    "identify a solution" in {
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
      empty.isSolution must beFalse
      contradiction.isSolution must beFalse
      prepped.isSolution must beTrue
    }

    "identify the first cell with minimal possibilities" in {
      val empty = SudokuGrid()
      empty.minimalPossibilityCell must beEqualTo(Some(0,0))
      val givens = SudokuGrid("500000000080000000000000000000000000000000000000000000000000000000000000000000000")
      givens.minimalPossibilityCell must beEqualTo(Some((0,1)))
      val removed = givens.removePossibility(5,5,1).removePossibility(5,5,2).removePossibility(5,5,3).removePossibility(5,5,4)
      removed.minimalPossibilityCell must beEqualTo(Some((5,5)))
      val solved = SudokuGrid("147258369258369147369147258471825936582936714693714825714582693825693471936471582")
      solved.minimalPossibilityCell must beEqualTo(None)
      val contradiction = ContradictorySudokuGrid
      contradiction.minimalPossibilityCell must beEqualTo(None)
    }

    "be able to fetch the list of possibilities" in {
      val givens = SudokuGrid("500000000080000000000000000000000000000000000000000000000000000000000000000000000")
      val removed = givens.removePossibility(5,5,1).removePossibility(5,5,2).removePossibility(5,5,3).removePossibility(5,5,4)
      removed.getPossibilities(5,5) must beEqualTo(List(5,6,7,8,9))
    }

    "generate a printable string for an empty grid" in {
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
      empty.toString() must beEqualTo(expectedstring)
    }

    "generate a printable string for a partially filled grid" in {
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
      placed.toString() must beEqualTo(expectedstring)
    }

    "recognize an empty puzzle" in {
      val empty = SudokuGrid()
      val contradiction = ContradictorySudokuGrid
      val nonempty = empty.placeConjecture(5,5,5)
      empty.isEmpty must beTrue
      contradiction.isEmpty must beFalse
      nonempty.isEmpty must beFalse
    }
  }

  "A Sudoku Solver" should {
    "do something reasonable for a contradictory puzzle" in {
      SudokuGrid.solve(ContradictorySudokuGrid) must beNone
    }

    "do something reasonable for an empty puzzle" in {
      SudokuGrid.solve(SudokuGrid()) must beNone
    }

    "solve an already solved puzzle" in {
      val solvedGiven = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
      val solvedGrid = SudokuGrid.solve(SudokuGrid(solvedGiven))
      solvedGrid must beSome
      solvedGrid.get.isSolution must beTrue
    }

    "solve a search puzzle" in {
      val unsolvedGiven = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
      val unsolvedGrid = SudokuGrid(unsolvedGiven)
      val expectedstring = "[4 1 7 | 3 6 9 | 8 2 5\n" +
        "6 3 2 | 1 5 8 | 9 4 7\n" +
        "9 5 8 | 7 2 4 | 3 1 6\n" +
        "---------------------\n" +
        "8 2 5 | 4 3 7 | 1 6 9\n" +
        "7 9 1 | 5 8 6 | 4 3 2\n" +
        "3 4 6 | 9 1 2 | 7 5 8\n" +
        "---------------------\n" +
        "2 8 9 | 6 4 3 | 5 7 1\n" +
        "5 7 3 | 2 9 1 | 6 8 4\n" +
        "1 6 4 | 8 7 5 | 2 9 3]"
      val solvedGrid = SudokuGrid.solve(unsolvedGrid)
      solvedGrid must beSome
      solvedGrid.get.isSolution must beTrue
      solvedGrid.get.toString() must beEqualTo(expectedstring)
    }
  }
}
