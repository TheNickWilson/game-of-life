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
}
