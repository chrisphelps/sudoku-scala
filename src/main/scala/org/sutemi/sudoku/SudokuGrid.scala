package org.sutemi.sudoku

/**
 * Created with IntelliJ IDEA.
 * User: chris
 * Date: 8/14/13
 * Time: 7:44 PM
 * To change this template use File | Settings | File Templates.
 */
class SudokuGrid {
    def getRowCells(row:Int, col:Int) =
      for(i <- 0 to 8 if i != col) yield (row,i)

    def getColCells(row:Int, col:Int) =
      for(i <- 0 to 8 if i != row) yield (i,col)
}
