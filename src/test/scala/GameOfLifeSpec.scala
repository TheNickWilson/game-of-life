import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, MustMatchers}

class GameOfLifeSpec extends FreeSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  implicit def noShrink[A]: Shrink[A] = Shrink.shrinkAny

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

  def genGameOfLife(size: Int, genCell: Gen[Cell] = arbitrary[Cell]): Gen[GameOfLife] =
    Gen.listOfN(size, Gen.listOfN(size, genCell))
      .map(s => GameOfLife(s))

  "a dead cell" - {

    "must stay dead" - {

      "when it has 2 or fewer living neighbours" in {

        val gen =
          for {
            size <- Gen.chooseNum(3, 100)
            game <- genGameOfLife(size, mainlyDeadCells)
            x    <- Gen.chooseNum(0, size - 1)
            y    <- Gen.chooseNum(0, size - 1)
          } yield (game.set(x, y, Dead), x, y)

        forAll(gen) {
          case (game, x, y) =>

            val neighbours = {
              for {
                xs <- (x - 1) to (x + 1)
                ys <- (y - 1) to (y + 1)
                if xs != x || ys != y
              } yield game.state.lift(ys).flatMap(_.lift(xs))
            }.flatten.toList

            val numberOfLivingNeighbours =
              neighbours.count(_ == Alive)

            whenever(numberOfLivingNeighbours <= 2) {
              game.next.state(y)(x) mustEqual Dead
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
          game  <- genGameOfLife(size)
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
