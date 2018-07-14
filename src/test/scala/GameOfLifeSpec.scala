import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, MustMatchers}

class GameOfLifeSpec extends FreeSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  implicit lazy val arbitraryCell: Arbitrary[Cell] =
    Arbitrary {
      Gen.oneOf(Alive, Dead)
    }

  def frequencyCell(deadFreq: Int, liveFreq: Int): Gen[Cell] =
    Gen.frequency(
      deadFreq -> Dead,
      liveFreq -> Alive
    )

  def mainlyDeadCells = frequencyCell(2, 1)

  "a dead cell" - {

    "must stay dead" - {

      "when it has 2 or fewer living neighbours" in {

        val gameGen: Gen[Seq[Seq[Cell]]] =
          Gen.listOfN(3, Gen.listOfN(3, mainlyDeadCells))

        forAll(gameGen) {
          state =>

            val middleRow = state(1)
            val patchedMiddleRow = middleRow.patch(1, Seq(Dead), 1)
            val newState = state.patch(1, Seq(patchedMiddleRow), 1)

            val numberOfLivingCells =
              newState.map(_.count(_ == Alive)).sum

            whenever(numberOfLivingCells <= 2) {
              val game = GameOfLife(newState)
              game.next.state(1)(1) mustEqual Dead
            }
        }
      }
    }
  }

  "set" - {

    "must set the cell at a particular coordinate" in {

      val gen = {
        for {
          cell  <- arbitrary[Cell]
          size  <- Gen.chooseNum(1, 100)
          game  <- Gen.listOfN(size, Gen.listOfN(size, arbitrary[Cell]))
                    .map(s => GameOfLife(s))
          x     <- Gen.chooseNum(0, size - 1)
          y     <- Gen.chooseNum(0, size - 1)
        } yield (game, cell, x, y)
      }

      forAll(gen) {
        case (game, cell, x, y) =>
          game.set(x, y, cell).state(y)(x) mustEqual cell
      }
    }
  }
}
