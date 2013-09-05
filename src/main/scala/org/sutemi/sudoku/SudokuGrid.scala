package org.sutemi.sudoku

/**
 * Created with IntelliJ IDEA.
 * User: chris
 * Date: 8/14/13
 * Time: 7:44 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class SudokuGrid {
  def removePossibility(row: Int, col: Int, possibility: Int): SudokuGrid
  def placeConjecture(row: Int, col: Int, conjecture: Int): SudokuGrid
  def placeConjectures(points: List[(Int,Int,Int)]): SudokuGrid
  def countPossibilities(row: Int, col: Int): Int
  def isSolution: Boolean
  def isEmpty: Boolean
  def minimalPossibilityCell: Option[(Int,Int)]
  def getPossibilities(row: Int, col: Int): List[Int]
}

object ContradictorySudokuGrid extends SudokuGrid {
  override def removePossibility(row: Int, col: Int, possibility: Int) = ContradictorySudokuGrid
  override def placeConjecture(row: Int, col: Int, conjecture: Int) = ContradictorySudokuGrid
  override def placeConjectures(points: List[(Int,Int,Int)]): SudokuGrid = ContradictorySudokuGrid
  override def countPossibilities(row: Int, col: Int) = 0
  override def isSolution = false
  override def isEmpty = false
  override def minimalPossibilityCell = None
  override def getPossibilities(row: Int, col: Int) = Nil
}

class LiveSudokuGrid(private val grid: IndexedSeq[IndexedSeq[Int]]) extends SudokuGrid {

    def this() = { this(for (i <- 0 until 81) yield for (j <- 1 to 9) yield j) }

    override def removePossibility(row: Int, col: Int, possibility: Int) = {
      val index = getIndex(row, col)
      if (grid(index).size == 1 && grid(index).contains(possibility))
        ContradictorySudokuGrid
      else {
        val removed = new LiveSudokuGrid(grid.updated(index, grid(index) diff List(possibility)))
        if (removed.countPossibilities(row, col) == 1 && this.countPossibilities(row, col) != 1)
          removed.placeConjecture(row, col, removed.remainingPossibility(row, col))
        else
          removed
      }
    }

    private def remainingPossibility(row: Int, col: Int) = {
      grid(getIndex(row, col))(0)
    }


    private def getIndex(row: Int, col: Int): Int = {
      row * 9 + col
    }

    override def placeConjecture(row: Int, col: Int, conjecture: Int) = {
      val index = getIndex(row, col)
      if (!grid(index).contains(conjecture))
        ContradictorySudokuGrid
      else {
        val eliminated = new LiveSudokuGrid(grid.updated(index, Vector(conjecture)))
        eliminated.propagated(row, col, conjecture)
      }
    }

    override def placeConjectures(points: List[(Int,Int,Int)]): SudokuGrid = {
      points match {
        case Nil => this
        case (row, col, conjecture) :: tail => placeConjecture(row, col, conjecture).placeConjectures(tail)
      }
    }

    private def propagated(row: Int, col: Int, conjecture: Int) = {
      val roweliminated = propagateElimination(SudokuGrid.getRowCells(row, col).toList, conjecture, this)
      val coleliminated = propagateElimination(SudokuGrid.getColCells(row, col).toList, conjecture, roweliminated)
      val peereliminated = propagateElimination(SudokuGrid.getPeerCells(row,col).toList, conjecture, coleliminated)
      peereliminated
    }

    private def propagateElimination(cells: List[(Int,Int)], conjecture: Int, thegrid: SudokuGrid): SudokuGrid = {
        cells match {
          case Nil => thegrid
          case (row,col) :: tail => propagateElimination(tail, conjecture, thegrid.removePossibility(row, col, conjecture))
        }
    }

    override def countPossibilities(row: Int, col: Int) = grid(getIndex(row, col)).size

    override def isSolution = grid.forall(_.length == 1)

    override def isEmpty = grid.forall(_.length == 9)

    override def minimalPossibilityCell = {
      val possibilityList = (for (i<-0 to 80) yield i) map (idx => (idx, grid(idx).length)) filter (_._2 > 1)
      if (possibilityList.isEmpty)
        None
      else {
        val index = possibilityList.minBy(_._2)._1
        Some((index / 9, index % 9))
      }
    }

    override def getPossibilities(row: Int, col: Int) = {
      grid(getIndex(row, col)).toList
    }

    private def genPartialString(cell: IndexedSeq[Int], index: Int): String = {
      val cellchar = if (cell.size == 1) cell(0) else "."
      val sep = if (index == 26 || index == 53) "\n---------------------\n"
                else if (index == 80) ""
                else if ((index + 1) % 9 == 0) "\n"
                else if ((index + 1) % 3 == 0) " | "
                else " "
      "" + cellchar + sep    // need empty string to be sure we are doing string concat instead of int addition
    }

    override def toString = {
      val expanded = grid.zipWithIndex
      expanded.foldLeft("[") ((str, tup) => str + genPartialString(tup._1, tup._2)) + "]"
    }
}

object SudokuGrid {
  def getRowCells(row: Int, col: Int) =
    for(i <- 0 to 8 if i != col) yield (row, i)

  def getColCells(row: Int, col: Int) =
    for(i <- 0 to 8 if i != row) yield (i, col)

  def getPeerCells(row: Int, col: Int) = {
    val rowmultiple = row / 3
    val colmultiple = col / 3
    for {
      i <- 0 until 3
      j <- 0 until 3
      if !((i == row % 3) && (j == col % 3))
    } yield (i + 3 * rowmultiple, j + 3 * colmultiple)
  }

  def apply(): SudokuGrid = new LiveSudokuGrid

  def apply(puzzle: String): SudokuGrid = {
    val givens = (for (i <- 0 to 80) yield i) zip (puzzle.toList) map (a =>(a._1 / 9, a._1 % 9, a._2.asDigit)) filter (_._3 > 0)
    val empty = new LiveSudokuGrid
    empty.placeConjectures(givens.toList)
  }

  def solve(puzzle: SudokuGrid): Option[SudokuGrid] = {
    if (puzzle == ContradictorySudokuGrid || puzzle.isEmpty)
      None
    else if (puzzle.isSolution)
      Some(puzzle)
    else puzzle.minimalPossibilityCell match {
      case None => None
      case Some((row, col)) => {
        val nextpuzzles = puzzle.getPossibilities(row, col) map (poss => puzzle.placeConjecture(row, col, poss))
        nextpuzzles.foldLeft(None: Option[SudokuGrid]) ((left, right) => left match {
          case Some(puzzle) => left
          case None => solve(right)
        })
      }
    }
  }
}
