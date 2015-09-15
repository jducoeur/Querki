package querki.core

import querki.test._
import querki.util.PublicException

/**
 * @author jducoeur
 */
class TypeTests extends QuerkiTests {
  
  lazy val Types = interface[querki.types.Types]
  
  "Number Properties" should {
    "validate correctly" in {
      class TSpace extends CommonSpace {
        val numProp = 
          new TestProperty(Core.IntType, ExactlyOne, "Num Prop",
                Types.MinIntValueProp(1),
                Types.MaxIntValueProp(12))
      }
      implicit val s = new TSpace
      
      val prop = s.state.prop(s.numProp).get
      // This will throw an exception if the value doesn't validate
      def validate(str:String) = {
        prop.validate(str, s.state)
      }
      def validateError(str:String, error:String) = {
        val thrown = the [PublicException] thrownBy validate(str)
        thrown.msgName should equal (error)
      }
      
      validate("1")
      validate("4")
      validate("12")
      
      validateError("0", "Types.Int.tooLow")
      validateError("13", "Types.Int.tooHigh")
      validateError("", "Types.Int.empty")
      validateError("floob", "Types.Int.badFormat")
    }
  }
}
