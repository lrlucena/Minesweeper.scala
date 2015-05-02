import Game1._

sealed abstract class Cell { val shown: Boolean }
case class Bomb(shown: Boolean = false) extends Cell
case class Empty(shown: Boolean = false) extends Cell
case class Hint(shown: Boolean = false, x: Int) extends Cell

object Game1 {
  type Board = List[List[Cell]]
  implicit class BoardUpdate(board: Board) {
    def update(x: Int, y: Int, value: Cell) = board.updated(x, board(x).updated(y, value))
} }

class Game(val rows: Int, val columns: Int, val bombs: Int, boardc: Option[Board] = None) {
  val board = boardc getOrElse initBoard

  lazy val initBoard = {
    val emptyBoard = List.fill(rows, columns) { Empty() }
    val bombsBoard = initBombs(bombs, emptyBoard)
    initHints(bombsBoard, rows, columns)
  }

  //bombs
  private def initBombs(quantity: Int, board: Board) = {
    val r = new java.util.Random
    val points = for (
      i <- 0 until rows;
      j <- 0 until columns
    ) yield (r.nextDouble -> (i, j))
    val bombs = points sortBy { _._1 } map { _._2 } take quantity
    bombs.foldLeft(board){
      case (board, (x,y)) => {
        board.update(x, y, Bomb(false))
  } } }
  //end of bombs

  //hints
  private def initHints(board: Board, rows: Int, columns: Int) =
    List.tabulate(rows, columns) { (x, y) =>
      board(x)(y) match {
        case Bomb(b) => Bomb(b)
        case _       => initHint(board, x, y)
    } }

  private def initHint(boardWithBombs: Board, x: Int, y: Int) = {
    val neighborCells = for (
      i <- x - 1 to x + 1;
      j <- y - 1 to y + 1;
      if i != x || j != y
    ) yield cellOnBoard(boardWithBombs)(i, j)
    val hintValue = neighborCells.flatten.map {
      case Bomb(_) => 1
      case _       => 0
    }.sum
    hintValue match {
      case 0 => Empty(false)
      case v => Hint(false, v)
  } }
  //end of hints

  //game behavior
  def showCell(x: Int, y: Int) = board(x)(y) match {
    case Empty(false)      => showNeighborCells(x, y)
    case Bomb(false)       => update(x, y, Bomb(true))
    case Hint(false, hint) => update(x, y, Hint(true, hint))
    case _                 => this
  }

  private def showNeighborCells(x: Int, y: Int): Game = {
    val neighborPositions = for (
      i <- x - 1 to x + 1;
      j <- y - 1 to y + 1;
      if contains(i, j) && (i != x || j != y)
    ) yield (i, j)

    val newGame = update(x, y, Empty(true))
    neighborPositions.foldLeft(newGame){
      case (game, (x,y)) => {
        game.showCell(x, y)
  } } }

  def hasOnlyBombs = !board.flatten.exists {
    case Empty(false) | Hint(false, _) => true
    case _                             => false
  }

  def hasActiveBomb = board.flatten.exists {
    case Bomb(true) => true
    case _          => false
  }

  def contains(x: Int, y: Int) =
    x >= 0 && x < rows && y >= 0 && y < columns

  def cell = cellOnBoard(board) _

  private def cellOnBoard(board: Board)(x: Int, y: Int) =
    if (contains(x, y)) Some(board(x)(y)) else None

  private def update(x: Int, y: Int, cell: Cell) =
    new Game(rows, columns, bombs, Some(board.update(x, y, cell)))
}
