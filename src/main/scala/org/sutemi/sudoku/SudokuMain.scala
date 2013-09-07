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
        solution match {
          case Some(solvedgrid) => if (solvedgrid.isSolution) println("Solved puzzle " + (l._2 + 1)) else println("Not solved")
          case None => println("Not solved")
        }
      }
      println("Solves done")
    }
    processFile("sudoku.txt")
    println("Easy solves done")
    processFile("top95.txt")
    println("Hard solves done")
  }
}