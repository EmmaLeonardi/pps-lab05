package polyglot.a05b

import polyglot.a05b.Logics

import scala.math.abs

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:
  private val dim = size
  private val random = scala.util.Random
  private val initialPosition = (random.nextInt(dim - 2) + 1, random.nextInt(dim - 2) + 1)
  private var counter = 0

  override def tick(): Unit = counter = counter + 1

  override def isOver: Boolean =
    initialPosition._2 - counter < 0 || initialPosition._2 + counter >= dim || initialPosition._1 - counter < 0 || initialPosition._1 + counter >= dim

  override def hasElement(x: Int, y: Int): Boolean =
    (x == initialPosition._1 && abs(y - initialPosition._2) <= counter) || (y == initialPosition._2 && abs(x - initialPosition._1) <= counter) |
      (x - y == initialPosition._1 - initialPosition._2 && abs(x - initialPosition._1) <= counter) || (x + y == initialPosition._1 + initialPosition._2 && abs(y - initialPosition._2) <= counter)