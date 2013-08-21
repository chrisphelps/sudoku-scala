package org.sutemi.sudoku

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

/**
 * Created with IntelliJ IDEA.
 * User: chris
 * Date: 8/14/13
 * Time: 7:52 PM
 * To change this template use File | Settings | File Templates.
 */
@RunWith(classOf[JUnitRunner])
class SudokuGridSpecification extends FunSpec with ShouldMatchers {
  describe("A SudokuGrid") {

    it("should find cells in the row") {
      val grid = new SudokuGrid
      assert(grid.getRowCells(1,3) === Vector((1,0),(1,1),(1,2),(1,4),(1,5),(1,6),(1,7),(1,8)))
    }

    it("should find cells in the column") {
      val grid = new SudokuGrid
      assert(grid.getColCells(1,3) === Vector((0,3),(2,3),(3,3),(4,3),(5,3),(6,3),(7,3),(8,3)))
    }

    it("should find peer cells in the unit") {
      val grid = new SudokuGrid
      assert(grid.getPeerCells(0,0) === Vector((0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)))
      assert(grid.getPeerCells(1,3) === Vector((0,3),(0,4),(0,5),(1,4),(1,5),(2,3),(2,4),(2,5)))
      assert(grid.getPeerCells(7,8) === Vector((6,6),(6,7),(6,8),(7,6),(7,7),(8,6),(8,7),(8,8)))
    }

    it("should remove a single possibility") {
      val grid = new SudokuGrid
      val newgrid = grid.removePossibility(3,4,5)
      newgrid should be ('defined)
      assert(newgrid.get.countPossibilities(3,4) === 8)
    }

    it("should remove many possibilities") {
      val empty = new SudokuGrid
      val removedPoss =
        for {
            a <- empty.removePossibility(0,0,1)
            b <- a.removePossibility(0,0,2)
            c <- b.removePossibility(0,0,3)
            d <- c.removePossibility(0,0,4)
            e <- d.removePossibility(0,0,5)
            f <- e.removePossibility(0,0,6)
            g <- f.removePossibility(0,0,7)
            h <- g.removePossibility(0,0,8)
        } yield h
      removedPoss should be ('defined)
      assert(removedPoss.get.countPossibilities(0,0) === 1)
    }

    it("should return none when removing only possibility") {
      val empty = new SudokuGrid
      val prepped =
        for {
            a <- empty.removePossibility(0,0,1)
            b <- a.removePossibility(0,0,2)
            c <- b.removePossibility(0,0,3)
            d <- c.removePossibility(0,0,4)
            e <- d.removePossibility(0,0,5)
            f <- e.removePossibility(0,0,6)
            g <- f.removePossibility(0,0,7)
            h <- g.removePossibility(0,0,8)
        } yield h
      prepped should be ('defined)
      val removed = prepped.get.removePossibility(0,0,9)
      removed should be (None)
    }

    it("should place a conjecture") {
      val empty = new SudokuGrid
      val placed = empty.placeConjecture(0,0,5)
      placed should be ('defined)
      assert(placed.get.countPossibilities(0,0) === 1)
    }

    it("should propagate a conjecture") {
      val empty = new SudokuGrid
      val placed = empty.placeConjecture(0,0,5)
      for (i <- 1 until 9) {
        assert(placed.get.countPossibilities(i,0) === 8)
        assert(placed.get.countPossibilities(0,i) === 8)
      }
    }

    it("should return none when conjecture is not possible") {
      val empty = new SudokuGrid
      val removed = empty.removePossibility(0,0,5)
      val placed = removed.get.placeConjecture(0,0,5)
      placed should be (None)
    }

    it("should place conjecture when all possibilities removed") {
      val empty = new SudokuGrid
      val prepped =
        for {
          a <- empty.removePossibility(0,0,1)
          b <- a.removePossibility(0,0,2)
          c <- b.removePossibility(0,0,3)
          d <- c.removePossibility(0,0,4)
          e <- d.removePossibility(0,0,5)
          f <- e.removePossibility(0,0,6)
          g <- f.removePossibility(0,0,7)
          h <- g.removePossibility(0,0,8)
        } yield h
      assert(prepped.get.countPossibilities(0,0) === 1)
      for (i <- 1 until 9) {
        assert(prepped.get.countPossibilities(0,i) === 8)
        assert(prepped.get.countPossibilities(i,0) === 8)
      }
    }
    // todo: test which does propagation and fails due to contradiction
  }
}
