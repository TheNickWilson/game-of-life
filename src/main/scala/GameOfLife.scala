sealed trait Cell
object Alive extends Cell
object Dead extends Cell

case class GameOfLife(state: Seq[Seq[Cell]]) {

  def next: GameOfLife = ???
}
