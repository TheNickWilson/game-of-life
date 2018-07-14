sealed trait Cell
object Alive extends Cell
object Dead extends Cell

case class GameOfLife(state: Seq[Seq[Cell]]) {

  def next: GameOfLife = this

  def set(x: Int, y: Int, cell: Cell): GameOfLife = {
    GameOfLife {
      state.updated(y, state(y).updated(x, cell))
    }
  }

  def get(x: Int, y: Int): Option[Cell] =
    state.lift(y).flatMap(_.lift(x))

  def neighbours(x0: Int, y0: Int): List[Cell] = {
    for {
      x <- (x0 - 1) to (x0 + 1)
      y <- (y0 - 1) to (y0 + 1)
      if x != x0 || y != y0
    } yield get(x, y)
  }.flatten.toList
}
