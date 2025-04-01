package polyglot.a01a

import util.Sequences.Sequence
import util.Sequences.Sequence.Cons
import polyglot.a01a.Logics.Result

trait LogicsScala extends Logics:
  def hit(row: Int, col: Int): Result

  object LogicsScala:
    def apply(size: Int, boat: Int): LogicsScala = LogicsImpl(size, boat)

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val size: Int, private val boat: Int) extends LogicsScala:

  private val maxTries = 5
  private var tries = 0
  private val sizeGrid = size
  private val random = scala.util.Random
  private val boatSize = boat
  private val boatPos = (random.nextInt(sizeGrid), random.nextInt(sizeGrid - boatSize + 1))
  private var cellsHit: Sequence[(Int, Int)] = Sequence.Nil() // sequence, int int
  println(boatPos)

  def hit(row: Int, col: Int): Result =
    if (row == boatPos._1 && col >= boatPos._2 && col < boatPos._2 + boatSize) {
      cellsHit = Cons((row, col), cellsHit)
      if cellsHit.lenght() == boatSize
      then Result.WON
      else Result.HIT
    } else {
      tries = tries + 1
      if tries == maxTries
      then Result.LOST
      else Result.MISS
    }
