package querki.core

import querki.globals._
import querki.test._
import querki.util.PublicException

/**
 * @author jducoeur
 */
class TypeTests extends QuerkiTests {
  
  lazy val Functions = interface[querki.core.Functions]
  lazy val Logic = interface[querki.logic.Logic]
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
    
    "be addable" in {
      class TSpace extends CommonSpace {
        val numProp = new TestProperty(Core.IntType, ExactlyOne, "Int Prop")
        val floatProp = new TestProperty(Core.FloatType, ExactlyOne, "Float Prop")
        val floatProp2 = new TestProperty(Core.FloatType, ExactlyOne, "Another Float Prop")
        
        val testThing = new SimpleTestThing("My Thing",
          numProp(39),
          floatProp(92.9),
          floatProp2(3.1))
      }
      implicit val s = new TSpace
      
      pql("[[My Thing -> Int Prop -> _plus(14)]]") should equal ("53")
      pql("[[My Thing -> Float Prop -> _plus(My Thing -> Another Float Prop)]]") should equal ("96.0")
    }
  }
  
  "Models" should {
    "be able to define abstract function implementations" in {
      class TSpace extends CommonSpace {
        import Core.IntType
        import querki.ql.Invocation
        import querki.values.QFut
        
        val Numerator = new TestProperty(IntType, ExactlyOne, "Numerator")
        val Denominator = new TestProperty(IntType, ExactlyOne, "Denominator")
        
        val Fraction = new SimpleTestThing("Fraction", Numerator(0), Denominator(1))
        
        val OneHalf = new TestThing("One Half", Fraction, Numerator(1), Denominator(2))
        val OnePointFive = new TestThing("One Point Five", Fraction, Numerator(3), Denominator(2))
        val Two = new TestThing("Two", Fraction, Numerator(2), Denominator(1))

        /**
         * We're defining an implementation for _plus over our hypothetical Fractions type.
         * 
         * Note that the user level doesn't actually have UI to do this yet. But we want to make
         * sure it works, so that users will be able to do it later.
         */
        val plusFunction = new TestThing("myPlusImpl", querki.core.FunctionMOIDs.ImplementationModelOID,
          Functions.ImplementsFunctionProp(Logic.PlusMethod.id),
          Functions.ImplementsTypesProp(Fraction),
          // We're going to totally cheat and just return the answer instead of calculating it,
          // because we honestly don't *care* about the calculation process, just the fact that
          // this qlApply() gets invoked.
          Basic.ApplyMethod("""Two""")
        )
      }
      implicit val s = new TSpace
      
      pql("""[[One Half -> _plus(OnePointFive)]]""") should equal (linkText(s.Two))
    }
  }
}
