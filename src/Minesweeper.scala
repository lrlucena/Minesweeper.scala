import java.util.Random
import Game1._
import scala.annotation.tailrec
sealed abstract class Cell {
  val shown: Boolean
}
case class Bomb(shown: Boolean = false) extends Cell
case class Empty(shown: Boolean = false) extends Cell
case class Hint(shown: Boolean = false, val x: Int) extends Cell

object Game1 {
  type Board = List[List[Cell]]
  implicit class BoardUpdate(board: Board) {
    def update(x: Int, y: Int, value: Cell) = board.updated(x, board(x).updated(y, value))
  }
}

class Game(val rows: Int, val columns: Int, val bombs: Int, boardc: Option[Board] = None) {
  val board = boardc.getOrElse(initBoard)

  lazy val initBoard = {
    val board1 = List.fill(rows, columns) { Empty() }
    val board2 = initBombs(bombs, board1)
    initHints(board2, rows, columns)
  }

  //bombs
  @tailrec private def initBombs(quantity: Int, board: Board): Board = {
    if (quantity > 0) {
      initBombs(quantity - 1, tryToPutBomb(board))
    } else {
      board
    }
  }

  @tailrec private def tryToPutBomb(board: Board): Board = {
    val r = new Random
    val x = r.nextInt(rows)
    val y = r.nextInt(columns)
    board(x)(y) match {
      case Bomb(_) => tryToPutBomb(board)
      case _ => board.update(x, y, Bomb())
    }
  }
  //end of bombs

  //hints
  private def initHints(board: Board, x: Int, y: Int) =
    List.tabulate(x, y) { transformIntoHint(board, _, _) }

  private def transformIntoHint(boardWithBombs: Board, x: Int, y: Int) = boardWithBombs(x)(y) match {
    case Bomb(b) => Bomb(b)
    case _ => initHint(boardWithBombs, x, y)
  }

  private def initHint(boardWithBombs: Board, x: Int, y: Int) = {
    val neighborCells =
      for (i <- -1 to 1; j <- -1 to 1 if i != 0 || j != 0) yield getCellOnBoard(boardWithBombs)(x + i, y + j)
    val hintValue = {
      neighborCells.flatten.map {
        case Bomb(_) => 1
        case _ => 0
      }.sum
    }
    hintValue match {
      case 0 => Empty(false)
      case _ => Hint(false, hintValue)
    }
  }
  //end of hints

  //game behavior
  def showCell(x: Int, y: Int): Game = {
    board(x)(y) match {
      case Empty(false) => showNeighborCells(x, y)
      case Bomb(false) => {
        val newboard = board.update(x, y, Bomb(true))
        new Game(rows, columns, bombs, Some(newboard))
      }
      case Hint(false, hint) => {
        val newboard = board.update(x, y, Hint(true, hint))
        new Game(rows, columns, bombs, Some(newboard))
      }
      case _ => this
    }
  }

  private def showNeighborCells(x: Int, y: Int): Game = {  
    def getPosition(board: Board, x: Int, y: Int) =
      if (contains(x, y)) Some((x, y)) else None

    val newboard = board.update(x, y, Empty(true))
    val newGame = new Game(rows, columns, bombs, Some(newboard))
    val neighborPositions =
      for (i <- -1 to 1; j <- -1 to 1 if i != 0 || j != 0) yield getPosition(board, x + i, y + j)
    neighborPositions.flatten.foldLeft(newGame)((game, tuple) => { val (x, y) = tuple; game.showCell(x, y) })
  }

  def hasOnlyBombs = !board.flatten.exists {
    case Empty(false) | Hint(false, _) => true
    case _ => false
  }

  def hasActiveBomb = board.flatten.exists {
    case Bomb(true) => true
    case _ => false
  }

  def contains(x: Int, y: Int) = {
    x >= 0 && x < rows && y >= 0 && y < columns
  }

  val getCell = getCellOnBoard(board) _

  def getCellOnBoard(board: Board)(x: Int, y: Int) = {
    if (contains(x, y)) Some(board(x)(y)) else None
  }
}
