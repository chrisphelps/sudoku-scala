package org.sutemi.sudoku

/**
 * Created with IntelliJ IDEA.
 * User: chris
 * Date: 8/14/13
 * Time: 7:44 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class SudokuGrid {
  def removePossibility(row:Int, col:Int, possibility:Int):SudokuGrid
  def placeConjecture(row:Int, col:Int, conjecture:Int):SudokuGrid
  def countPossibilities(row:Int, col:Int):Int
}

object ContradictorySudokuGrid extends SudokuGrid {
  override def removePossibility(row:Int, col:Int, possibility:Int) = ContradictorySudokuGrid
  override def placeConjecture(row:Int, col:Int, conjecture:Int) = ContradictorySudokuGrid
  override def countPossibilities(row:Int, col:Int) = 0
}

object SudokuGrid {
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
}

class LiveSudokuGrid(private val grid: IndexedSeq[IndexedSeq[Int]]) extends SudokuGrid {

    def this() = { this(for (i <- 0 until 81) yield for (j <- 1 to 9) yield j) }

    override def removePossibility(row:Int, col:Int, possibility: Int) = {
      val index = getIndex(row, col)
      if (grid(index).size == 1 && grid(index).contains(possibility))
        ContradictorySudokuGrid
      else {
        val removed = new LiveSudokuGrid(grid.updated(index,grid(index) diff List(possibility)))
        if (removed.countPossibilities(row,col) == 1)
          removed.placeConjecture(row,col,removed.remainingPossibility(row,col))
        else
          removed
      }
    }

    private def remainingPossibility(row:Int, col:Int) = {
      grid(getIndex(row, col))(0)
    }


    private def getIndex(row: Int, col: Int): Int = {
      row * 9 + col
    }

    override def placeConjecture(row:Int, col:Int, conjecture:Int) = {
      val index = getIndex(row, col)
      if (!grid(index).contains(conjecture))
        ContradictorySudokuGrid
      else {
        val eliminated = new LiveSudokuGrid(grid.updated(index,Vector(conjecture)))
        eliminated.propagated(row,col,conjecture)
      }
    }

//    private def propagateConjectures(points:List[(Int,Int,Int)],grid:Option[SudokuGrid]) = {
//      grid match {
//        case None => None
//
//      }
//    }

    def placeConjectures(points:List[(Int,Int,Int)]):SudokuGrid = {
      this
    }

    private def propagated(row:Int, col:Int, conjecture:Int) = {
      val roweliminated = propagateElimination(SudokuGrid.getRowCells(row,col).toList, conjecture, this)
      val coleliminated = propagateElimination(SudokuGrid.getColCells(row,col).toList, conjecture, roweliminated)
      val peereliminated = propagateElimination(SudokuGrid.getPeerCells(row,col).toList, conjecture, coleliminated)
      peereliminated
    }

    private def propagateElimination(cells:List[(Int,Int)], conjecture:Int, thegrid:SudokuGrid):SudokuGrid = {
        cells match {
          case Nil => thegrid
          case (row,col)::tail => propagateElimination(tail,conjecture,thegrid.removePossibility(row,col,conjecture))
        }
    }

    override def countPossibilities(row:Int, col:Int) = grid(getIndex(row, col)).size
}
