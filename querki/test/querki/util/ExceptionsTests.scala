package querki.util

import querki.test._

class ExceptionsTests extends QuerkiTests {
  "TryTrans" should {
    "work with a success case" in {
      def myFunc = { 3 }
      
      val result = TryTrans { myFunc } onSucc { _.toString } onFail { _.msgName }
      result.result should equal ("3")
    }
    
    "work with a PublicException" in {
      def myFunc = { throw new PublicException("Ex1"); 3 }
      
      val result = TryTrans { myFunc } onSucc { _.toString } onFail { _.msgName }
      result.result should equal ("Ex1")      
    }
    
    "work with a random Exception" in {
      def myFunc = { throw new Exception("Ex1"); 3 }
      
      val result = TryTrans { myFunc } onSucc { _.toString } onFail { _.msgName }
      result.result should equal ("General")      
    }
  }
}
