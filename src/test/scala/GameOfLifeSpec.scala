import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, MustMatchers}

class GameOfLifeSpec extends FreeSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  "a dead cell" - {

    "must stay dead" - {

      "when it has 2 or fewer living neighbours" ignore {

      }
    }
  }
}
