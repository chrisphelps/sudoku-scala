package org.sutemi.sudoku

/**
 * Created with IntelliJ IDEA.
 * User: chris
 * Date: 8/14/13
 * Time: 7:44 PM
 * To change this template use File | Settings | File Templates.
 */
class SudokuGrid(private val grid: IndexedSeq[IndexedSeq[Int]]) {

    def this() = { this(for (i <- 0 until 81) yield for (j <- 1 to 9) yield j) }

    def getRowCells(row:Int, col:Int) =
      for(i <- 0 to 8 if i != col) yield (row,i)

    def getColCells(row:Int, col:Int) =
      for(i <- 0 to 8 if i != row) yield (i,col)

    def getPeerCells(row:Int, col:Int) = {
      val rowmultiple = row / 3
      val colmultiple = col / 3
      for { i <- 0 until 3
           j <- 0 until 3
          if !((i == row % 3) && (j == col % 3))}
        yield (i + 3 * rowmultiple, j + 3 * colmultiple)
    }

    def removePossibility(row:Int, col:Int, possibility: Int) = {
      val index = row * 9 + col
      if (grid(index).size == 1 && grid(index).contains(possibility))
        None
      else
        Some(new SudokuGrid(grid.updated(index,grid(index) diff List(possibility))))
    }

    def countPossibilities(row:Int, col:Int) = grid(row * 9 + col).size
}
