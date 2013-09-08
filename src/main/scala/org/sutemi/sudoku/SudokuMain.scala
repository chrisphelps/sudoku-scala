package org.sutemi.sudoku

import scala.io.Source
/**
 * Created with IntelliJ IDEA.
 * User: chris
 * Date: 9/5/13
 * Time: 8:35 PM
 * To change this template use File | Settings | File Templates.
 */
object SudokuMain {
  def main(args: Array[String]) {
    def processFile(filename:String) {
      val source = Source.fromFile(filename)
      val lines = source.getLines

      val lineswithindex = lines.zipWithIndex

      for (l <- lineswithindex) {
        val solution = SudokuGrid.solve(SudokuGrid(l._1))
        val status = solution match {
          case Some(solvedgrid) =>
            if (solvedgrid.isSolution) "Solved puzzle " + (l._2 + 1)
            else "Not solved"
          case None => "Not solved"
        }
        println(status)
      }
      println("Solves done")
    }
    processFile("sudoku.txt")
    println("Easy solves done")
    processFile("top95.txt")
    println("Hard solves done")
  }
}