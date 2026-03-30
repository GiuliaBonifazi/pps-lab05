package it.unibo.pps.polyglot.a01b

import it.unibo.pps.polyglot.OptionToOptional
import it.unibo.pps.polyglot.a01b.Logics
import it.unibo.pps.util.Optionals.Optional as ScalaOptional
import it.unibo.pps.util.Sequences.*
import Sequence.*

import scala.annotation.tailrec
import scala.jdk.javaapi.OptionConverters

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  private var cellsUntilWon = size * size - mines
  private var lost = false
  private var minePositions: Sequence[(Int, Int)] = Nil()
  for i <- 1 to mines do {
    minePositions = Cons(spawnMine, minePositions)
  }
  println(minePositions)

  @tailrec
  private def spawnMine: (Int, Int) =
    val random = scala.util.Random()
    val newMine = (random.nextInt(size - 1), random.nextInt(size - 1))
    if minePositions.contains(newMine) then spawnMine else newMine
    
  private def checkAdjacentCoordinate(possibleAdjacent: Int, target: Int): Boolean =
    possibleAdjacent == target - 1 || possibleAdjacent == target || possibleAdjacent == target + 1
  
  @tailrec
  private def calculateAdjacentMines(using cell: (x: Int, y: Int))(minesPos: Sequence[(Int, Int)], acc: Int): Int =
    minesPos match
      case Cons((mineX, mineY), tail) if checkAdjacentCoordinate(mineX, cell.x) && checkAdjacentCoordinate(mineY, cell.y) 
        => calculateAdjacentMines(tail, acc + 1)
      case Cons(_, tail) => calculateAdjacentMines(tail, acc)
      case Nil() => acc

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    if (minePositions.contains((x, y)))
      lost = true
      OptionToOptional(ScalaOptional.Empty())
    else
      cellsUntilWon = cellsUntilWon - 1
      given cell: (Int, Int) = (x, y)
      OptionToOptional(ScalaOptional.Just(calculateAdjacentMines(minePositions, 0)))


  override def won: Boolean = cellsUntilWon == 0 && !lost
