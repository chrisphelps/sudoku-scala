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
      else {
        val removed = new SudokuGrid(grid.updated(index,grid(index) diff List(possibility)))
        if (removed.countPossibilities(row,col) == 1)
          removed.placeConjecture(row,col,removed.remainingPossibility(row,col))
        else
          Some(removed)
      }
    }

    private def remainingPossibility(row:Int, col:Int) = {
      val index = row * 9 + col
      grid(index)(0)
    }

    def placeConjecture(row:Int, col:Int, conjecture:Int) = {
      val index = row * 9 + col
      if (!grid(index).contains(conjecture))
        None
      else {
        val eliminated = new SudokuGrid(grid.updated(index,Vector(conjecture)))
        eliminated.propagated(row,col,conjecture)
      }
    }

    private def propagated(row:Int, col:Int, conjecture:Int) = {
      val roweliminated = propagateElimination(getRowCells(row,col).toList, conjecture, Some(this))
      val coleliminated = propagateElimination(getColCells(row,col).toList, conjecture, roweliminated)
      val peereliminated = propagateElimination(getPeerCells(row,col).toList, conjecture, coleliminated)
      peereliminated
    }

    private def propagateElimination(cells:List[(Int,Int)], conjecture:Int, grid:Option[SudokuGrid]):Option[SudokuGrid] = {
      grid match {
        case None => None
        case Some(thegrid) => cells match {
          case Nil => Some(thegrid)
          case (row,col)::tail => propagateElimination(tail,conjecture,thegrid.removePossibility(row,col,conjecture))
        }
      }
    }

    def countPossibilities(row:Int, col:Int) = grid(row * 9 + col).size
}
