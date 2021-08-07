package querki.imexport {

  import querki.test._

  import fakefastparse.all._

  /**
   * @author jducoeur
   */
  class FastParseTests extends QuerkiTests {
    "My mock FastParse" should {
      "compile without warnings" in {
        // Commented out so that we don't get spurious warnings. Uncomment to
        // see the warnings happen:
//        val r:fakefastparse.core.Result[_] = Result.Failure()
//
//        // Here is where the warnings come in:
//        r match {
//          case Result.Success() => fail("Okay, that totally didn't work")
//          case Result.Failure() => println(s"This worked as expected")
//        }
      }
    }
  }
}

package fakefastparse {

  package core {
    sealed trait Result[+T] {}

    object Result {
      case class Success[+T]() extends Result[T]
      case class Failure() extends Result[Nothing]
    }
  }

  trait Api {
    val Result = core.Result
  }

  object all extends Api
}
