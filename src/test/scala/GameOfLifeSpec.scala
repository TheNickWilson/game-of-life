import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, MustMatchers}

class GameOfLifeSpec extends FreeSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  implicit lazy val arbitraryCell: Arbitrary[Cell] =
    Arbitrary {
      Gen.oneOf(Alive, Dead)
    }

  "a dead cell" - {

    "must stay dead" - {

      "when it has 2 or fewer living neighbours" in {

        val gameGen: Gen[Seq[Seq[Cell]]] =
          Gen.listOfN(3, Gen.listOfN(3, arbitrary[Cell]))

        forAll(gameGen) {
          state =>

            val middleRow = state(1)
            val patchedMiddleRow = middleRow.patch(1, Seq(Dead), 1)
            val newState = state.patch(1, Seq(patchedMiddleRow), 1)

            val game = GameOfLife(newState)
            game.next.state(1)(1) mustEqual Dead
        }
      }
    }
  }
}
