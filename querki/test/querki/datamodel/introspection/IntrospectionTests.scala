package querki.datamodel.introspection

import querki.test._

import querki.types.{ModelTypeDefiner, SimplePropertyBundle}

class IntrospectionTests extends QuerkiTests {
  lazy val Editor = interface[querki.editing.Editor]
  class TSpace extends CommonSpace with ModelTypeDefiner {
    val prop1 = new TestProperty(Core.IntType, ExactlyOne, "First Prop")
    val prop2 = new TestProperty(Core.IntType, ExactlyOne, "Second Prop")
    val prop3 = new TestProperty(Core.IntType, ExactlyOne, "Third Prop")
    val prop4 = new TestProperty(Core.IntType, ExactlyOne, "Fourth Prop")
    val prop5 = new TestProperty(Core.IntType, ExactlyOne, "Fifth Prop")
    
    // props intentionally not quite in either alphabetical or numeric order:
    val bottomModel = new SimpleTestThing("Bottom Model", prop1(1), prop2(2), prop4(4), prop5(5), prop3(3),
        Editor.InstanceProps(prop1, prop2, prop3, prop4, prop5))
    
    val bottomThing1 = new TestThing("Bottom 1", bottomModel)
    val bottomThing2 = new TestThing("Bottom 2", bottomModel)
    
    val modelType = new ModelType(toid, bottomModel.id, 
      Core.toProps(
        Core.setName("__ Bottom Model Type")
      ))
    registerType(modelType)

    val propOfModelType = new TestProperty(modelType, ExactlyOne, "Complex Prop")
    
    val complexThingModel = new SimpleTestThing("Complex Thing Model", propOfModelType(SimplePropertyBundle()))
    val complexThing = new TestThing("My Complex Thing", complexThingModel, propOfModelType(SimplePropertyBundle(prop3(9))))
    val trivialComplexThing = new TestThing("Other Complex Thing", complexThingModel)
  }
  
  "_foreachProperty" should {
    "work for a simple Model Value" in {
      implicit val space = new TSpace
      
      // Note that the results are in order by Instance Props, not alphabetical:
      pql("""[[My Complex Thing -> Complex Prop -> _foreachProperty(""Name: [[_prop -> Name]]; Value: [[_val]]"") -> _bulleted]]""") should
        equal("""
            |* Name: First Prop; Value: 1
            |* Name: Second Prop; Value: 2
            |* Name: Third Prop; Value: 9
            |* Name: Fourth Prop; Value: 4
            |* Name: Fifth Prop; Value: 5""".stripReturns)
    }
  }
  
  "_isInherited" should {
    "work properly" in {
      implicit val space = new TSpace
      
      pql("""[[My Complex Thing -> Complex Prop -> _foreachProperty(_isInherited) -> _commas]]""") should
        equal("true, true, false, true, true")
    }
  }
  
  "_definedOn" should {
    "work properly" in {
      implicit val space = new TSpace
      
      pql("""[[Bottom Model._instances -> _sort -> _foreachProperty(""Name: [[_prop -> Name]]; Value: [[_val]]; On: [[_definedOn -> Name]]"") -> _bulleted]]""")  should
        equal ("""
            |* Name: First Prop; Value: 1; On: Bottom 1
            |* Name: Second Prop; Value: 2; On: Bottom 1
            |* Name: Third Prop; Value: 3; On: Bottom 1
            |* Name: Fourth Prop; Value: 4; On: Bottom 1
            |* Name: Fifth Prop; Value: 5; On: Bottom 1
            |* Name: First Prop; Value: 1; On: Bottom 2
            |* Name: Second Prop; Value: 2; On: Bottom 2
            |* Name: Third Prop; Value: 3; On: Bottom 2
            |* Name: Fourth Prop; Value: 4; On: Bottom 2
            |* Name: Fifth Prop; Value: 5; On: Bottom 2""".stripReturns)
    }    
  }
}