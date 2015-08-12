package querki.imexport {

  import querki.test._
  
  import fakefastparse.all._
  
  /**
   * @author jducoeur
   */
  class FastParseTests extends QuerkiTests {
    "My mock FastParse" should {
      "compile without warnings" in {
        val r:fakefastparse.core.Result[_] = Result.Failure("blah", 1)
        
        // Here is where the warnings come in:
        r match {
          case Result.Success() => fail("Okay, that totally didn't work")
          case Result.Failure(msg) => println(s"This worked as expected -- got $msg")
        }
      }
    }
  }
}

package fakefastparse {
  package core {
    sealed trait Result[+T] {}
    
    object Result {
      case class Success[+T]() extends Result[T]
      case class Failure(thingy:String, n:Int) extends Result[Nothing]
      
      object Failure {
        def unapply[T](x:Result[T]) = x match {
          case s:Failure => Some((s.thingy))
          case _ => None
        }
      }
    }
  }
  
  trait Api {
    val Result = core.Result
  }
  
  object all extends Api
}