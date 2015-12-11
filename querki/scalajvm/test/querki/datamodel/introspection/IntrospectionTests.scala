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
    
    val optProp = new TestProperty(Core.IntType, Optional, "Opt Prop")
    val optProp2 = new TestProperty(Core.IntType, Optional, "Opt Prop 2")
    val optProp3 = new TestProperty(Core.IntType, Optional, "Opt Prop 3")
    
    val modelWithOpt = new SimpleTestThing("Model with Opt", prop1(1), prop2(2), optProp(Core.QNone),
        Editor.InstanceProps(prop1, optProp, prop2))
    val thingWithOpt = new TestThing("Thing with Opt", modelWithOpt)
    
    val modelWithOpts = new SimpleTestThing("Model with Opts", optProp(0), optProp2(0), optProp3(0),
        Editor.InstanceProps(optProp, optProp2, optProp3))
    val thingWithOpts = new TestThing("Thing with Opts", modelWithOpts, optProp(Core.QNone), optProp2(4), optProp3(Core.QNone))    
    
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
            |<ul>
            |<li class="_bullet">
            |Name: First Prop; Value: 1
            |</li>
            |<li class="_bullet">
            |Name: Second Prop; Value: 2
            |</li>
            |<li class="_bullet">
            |Name: Third Prop; Value: 9
            |</li>
            |<li class="_bullet">
            |Name: Fourth Prop; Value: 4
            |</li>
            |<li class="_bullet">
            |Name: Fifth Prop; Value: 5
            |</li>
            |</ul>""".stripReturns)
    }
    
    "work with a non-quoted Expression" in {
      implicit val space = new TSpace
      
      pql("""[[Thing with Opt -> _foreachProperty(_if(_val -> _isNonEmpty, ""[[_prop -> Name]]: [[_val]]""))]]""") should
        equal("""
            |First Prop: 1
            |Second Prop: 2""".stripReturns)
      pql("""[[My Complex Thing -> Complex Prop -> _foreachProperty(_if(_isNonEmpty, ""[[_prop -> Name]]: [[_val]]""))]]""") should
        equal("""
            |First Prop: 1
            |Second Prop: 2
            |Third Prop: 9
            |Fourth Prop: 4
            |Fifth Prop: 5""".stripReturns)
    }
    
    "work with empty Optionals" in {
      implicit val space = new TSpace
      
      // Note that the results are in order by Instance Props, not alphabetical:
      pql("""[[Thing with Opts -> _foreachProperty(_if(_val -> _isNonEmpty, _val)) -> _bulleted]]""") should
        equal("""
            |<ul>
            |<li class="_bullet">
            |4
            |</li>
            |</ul>""".stripReturns)
    }
    
    "work without a parameter" in {
      implicit val space = new TSpace
      
      // Note that the results are in order by Instance Props, not alphabetical:
      pql("""[[My Complex Thing -> Complex Prop -> _foreachProperty]]""") should
        equal("""
            |: First Prop : 1 (inherited)
            |: Second Prop : 2 (inherited)
            |: Third Prop : 9
            |: Fourth Prop : 4 (inherited)
            |: Fifth Prop : 5 (inherited)""".stripReturns)
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
      
      pql("""[[Bottom Model._instances -> _foreachProperty(""Name: [[_prop -> Name]]; Value: [[_val]]; On: [[_definedOn -> Name]]"") -> _bulleted]]""")  should
        equal ("""
            |<ul>
			|<li class="_bullet">
			|Name: First Prop; Value: 1; On: Bottom 1
			|</li>
			|<li class="_bullet">
			|Name: Second Prop; Value: 2; On: Bottom 1
			|</li>
			|<li class="_bullet">
			|Name: Third Prop; Value: 3; On: Bottom 1
			|</li>
			|<li class="_bullet">
			|Name: Fourth Prop; Value: 4; On: Bottom 1
			|</li>
			|<li class="_bullet">
			|Name: Fifth Prop; Value: 5; On: Bottom 1
			|</li>
			|<li class="_bullet">
			|Name: First Prop; Value: 1; On: Bottom 2
			|</li>
			|<li class="_bullet">
			|Name: Second Prop; Value: 2; On: Bottom 2
			|</li>
			|<li class="_bullet">
			|Name: Third Prop; Value: 3; On: Bottom 2
			|</li>
			|<li class="_bullet">
			|Name: Fourth Prop; Value: 4; On: Bottom 2
			|</li>
			|<li class="_bullet">
			|Name: Fifth Prop; Value: 5; On: Bottom 2
			|</li>
			|</ul>""".stripReturns)
    }    
  }
}