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

  def genGameOfLife(size: Int, genCell: Gen[Cell] = arbitrary[Cell]): Gen[GameOfLife] =
    Gen.listOfN(size, Gen.listOfN(size, genCell))
      .map(s => GameOfLife(s))

  def genGameWithCoords(genCell: Gen[Cell] = arbitrary[Cell]): Gen[(GameOfLife, Int, Int)] =
    for {
      size <- Gen.chooseNum(3, 100)
      game <- genGameOfLife(size, genCell)
      x    <- Gen.chooseNum(0, size - 1)
      y    <- Gen.chooseNum(0, size - 1)
    } yield (game, x, y)

  "a dead cell" - {

    "must stay dead" - {

      "when it does not have exactly three living neighbours" in {

        forAll(genGameWithCoords()) {
          case (game, x, y) =>

            whenever(game.livingNeighbours(x, y) != 3) {
              game
                .set(x, y, Dead)
                .next
                .get(x, y).value mustEqual Dead
            }
        }
      }
    }

    "must become alive if it has exactly 3 live neighbours" in {

      forAll(genGameWithCoords(frequencyCell(6, 4))) {
        case (game, x, y) =>

          whenever(game.livingNeighbours(x, y) == 3) {
            game
              .set(x, y, Dead)
              .next
              .get(x, y).value mustEqual Alive
          }
      }
    }
  }

  "a living cell" - {

    "must live on" - {

      "if it has 2 or 3 live neighbours" in {

        forAll(genGameWithCoords(frequencyCell(11, 5))) {
          case (game, x, y) =>

            whenever(game.livingNeighbours(x, y) == 2 || game.livingNeighbours(x, y) == 3) {
              game
                .set(x, y, Alive)
                .next
                .get(x, y).value mustEqual Alive
            }
        }
      }
    }

    "must die" - {

      "if it has fewer than 2 live neighbours" in {

        forAll(genGameWithCoords(frequencyCell(7, 1))) {
          case (game, x, y) =>

            whenever(game.livingNeighbours(x, y) < 2) {
              game
                .set(x, y, Alive)
                .next
                .get(x, y).value mustEqual Dead
            }
        }
      }

      "if it has more than 3 live neighbours" in {

        forAll(genGameWithCoords(frequencyCell(3, 5))) {
          case (game, x, y) =>

            whenever(game.livingNeighbours(x, y) > 3) {
              game
                .set(x, y, Alive)
                .next
                .get(x, y).value mustEqual Dead
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
