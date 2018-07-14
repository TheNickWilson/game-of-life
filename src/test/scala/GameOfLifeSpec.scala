import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, MustMatchers, OptionValues}

class GameOfLifeSpec extends FreeSpec with MustMatchers with GeneratorDrivenPropertyChecks with OptionValues {

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
  def mainlyLivingCells = frequencyCell(1, 2)

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

            whenever(game.neighbours(x, y).count(_ == Alive) <= 2) {
              game.next.get(x, y).value mustEqual Dead
            }
        }
      }

      "when it has more than 3 living neighbours" in {

        val gen =
          for {
            size <- Gen.chooseNum(3, 100)
            game <- genGameOfLife(size, mainlyLivingCells)
            x    <- Gen.chooseNum(0, size - 1)
            y    <- Gen.chooseNum(0, size - 1)
          } yield (game.set(x, y, Dead), x, y)

        forAll(gen) {
          case (game, x, y) =>

            whenever(game.neighbours(x, y).count(_ == Alive) > 2) {
              game.next.get(x, y).value mustEqual Dead
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

  "get" - {

    "must return `None`" - {

      "when a coordinate doesn't exist in a game" in {

        val gen = for {
          size <- Gen.chooseNum(1, 100)
          game <- genGameOfLife(size)
          x    <- arbitrary[Int]
          y    <- arbitrary[Int]
        } yield (size, game, x, y)

        forAll(gen) {
          case (size, game, x, y) =>
            whenever((x < 0 || x > size) && (y < 0 || y > size)) {
              game.get(x, y) mustNot be (defined)
            }
        }
      }
    }

    "must return the cell at that coordinate" in {

      val gen = for {
        size <- Gen.chooseNum(1, 100)
        game <- genGameOfLife(size)
        x    <- Gen.chooseNum(0, size - 1)
        y    <- Gen.chooseNum(0, size - 1)
        cell <- arbitrary[Cell]
      } yield (game, x, y, cell)

      forAll(gen) {
        case (game, x, y, cell) =>
          game.set(x, y, cell).get(x, y).value mustEqual cell
      }
    }
  }

  "neighbours" - {

    "must return a list of neighbours" in {

      val gen = for {
        size   <- Gen.chooseNum(1, 100)
        game   <- genGameOfLife(size)
        x      <- Gen.oneOf(0, size - 1)
        y      <- Gen.oneOf(0, size - 1)
      } yield (game, x, y)

      forAll(gen) {
        case (game, x, y) =>

          val neighbours = Seq(
            game.get(x - 1, y - 1),
            game.get(x    , y - 1),
            game.get(x + 1, y - 1),
            game.get(x - 1, y),
            game.get(x + 1, y),
            game.get(x - 1, y + 1),
            game.get(x    , y + 1),
            game.get(x + 1, y + 1)
          ).flatten

          game.neighbours(x, y) must contain theSameElementsAs neighbours
      }
    }
  }
}
