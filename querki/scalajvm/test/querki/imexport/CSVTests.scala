package querki.imexport

import querki.test._

import querki.ecology._
import querki.types.{ModelTypeDefiner, SimplePropertyBundle}

class CSVTests extends QuerkiTests {
  lazy val Imexport = interface[querki.imexport.Imexport]
  
  "exporting a CSV" should {
    "work for a simple Model" in {
      class TSpace extends CommonSpace {
        val numberProp = new TestProperty(Core.IntType, ExactlyOne, "Number Prop")
        val textProp = new TestProperty(TextType, ExactlyOne, "Text Prop")
        val myModel = new SimpleTestThing("My Model", numberProp(42), textProp("Hello"))
        
        val instance1 = new TestThing("Instance 1", myModel)
        val instance2 = new TestThing("Instance 2", myModel, numberProp(99))
        val instance3 = new TestThing("Instance 3", myModel, textProp("there"))
      }
      
      implicit val s = new TSpace
      
      val result = Imexport.exportInstances(getRc, Format.CSV, s.myModel)(s.state)
      assert(result.name == "My Model.csv")
      // No Instance Properties specified, so we show the Properties in alphabetical
      // order: Name, Number Prop, Text Prop. Everything is ExactlyOne, so no empty fields:
      new String(result.content).stripReturns should equal("""Link Name,Number Prop,Text Prop
          |Instance-1,42,Hello
          |Instance-2,99,Hello
          |Instance-3,42,there""".stripReturns)
    }

    "work with Instance Props set" in {
      class TSpace extends CommonSpace {
        val numberProp = new TestProperty(Core.IntType, ExactlyOne, "Number Prop")
        val textProp = new TestProperty(TextType, ExactlyOne, "Text Prop")
        val myModel = new SimpleTestThing("My Model", numberProp(42), textProp("Hello"), 
            interface[querki.editing.Editor].InstanceProps(textProp, Core.NameProp))
        
        val instance1 = new TestThing("Instance 1", myModel)
        val instance2 = new TestThing("Instance 2", myModel, numberProp(99))
        val instance3 = new TestThing("Instance 3", myModel, textProp("there"))
      }
      
      implicit val s = new TSpace
      
      val result = Imexport.exportInstances(getRc, Format.CSV, s.myModel)(s.state)
      assert(result.name == "My Model.csv")
      new String(result.content).stripReturns should equal("""Text Prop,Link Name
          |Hello,Instance-1
          |Hello,Instance-2
          |there,Instance-3""".stripReturns)
    }

    "work with an empty Optional" in {
      class TSpace extends CommonSpace {
        val numberProp = new TestProperty(Core.IntType, ExactlyOne, "Number Prop")
        val textProp = new TestProperty(TextType, Optional, "Text Prop")
        val myModel = new SimpleTestThing("My Model", numberProp(42), textProp(Core.QNone))
        
        val instance1 = new TestThing("Instance 1", myModel)
        val instance2 = new TestThing("Instance 2", myModel, numberProp(99))
        val instance3 = new TestThing("Instance 3", myModel, textProp("there"))
      }
      
      implicit val s = new TSpace
      
      val result = Imexport.exportInstances(getRc, Format.CSV, s.myModel)(s.state)
      assert(result.name == "My Model.csv")
      // No Instance Properties specified, so we show the Properties in alphabetical
      // order: Name, Number Prop, Text Prop. Everything is ExactlyOne, so no empty fields:
      new String(result.content).stripReturns should equal("""Link Name,Number Prop,Text Prop
          |Instance-1,42,
          |Instance-2,99,
          |Instance-3,42,there""".stripReturns)
    }
    
    // IMPORTANT: this currently does the default behaviour, of producing a single cell per
    // List element, and only putting in the first value. This is potentially subject to change,
    // and will almost certainly be overrideable in the medium term.
    "work with a List" in {
      class TSpace extends CommonSpace {
        val numberProp = new TestProperty(Core.IntType, ExactlyOne, "Number Prop")
        val textProp = new TestProperty(TextType, QList, "Text Prop")
        val myModel = new SimpleTestThing("My Model", numberProp(42), textProp("Hello"))
        
        val instance1 = new TestThing("Instance 1", myModel)
        val instance2 = new TestThing("Instance 2", myModel, textProp())
        val instance3 = new TestThing("Instance 3", myModel, textProp("Hi", "there"))
      }
      
      implicit val s = new TSpace
      
      val result = Imexport.exportInstances(getRc, Format.CSV, s.myModel)(s.state)
      assert(result.name == "My Model.csv")
      // No Instance Properties specified, so we show the Properties in alphabetical
      // order: Name, Number Prop, Text Prop. Everything is ExactlyOne, so no empty fields:
      new String(result.content).stripReturns should equal("""Link Name,Number Prop,Text Prop
          |Instance-1,42,Hello
          |Instance-2,42,
          |Instance-3,42,Hi""".stripReturns)
    }
    
    "work for a nested Model" in {
      class TSpace extends CommonSpace with ModelTypeDefiner {
		  val numberProp = new TestProperty(Core.IntType, ExactlyOne, "Number in Model")
		  val textProp = new TestProperty(TextType, ExactlyOne, "Text in Model")
		    
		  val modelForType = new SimpleTestThing("Model for Type",
		      numberProp(0),
		      textProp(""), 
              interface[querki.editing.Editor].InstanceProps(textProp, numberProp))
		    
		  val modelType = new ModelType(toid, modelForType.id, 
		      Core.toProps(
		        Core.setName("My Model Type")
		          ))
		  registerType(modelType)
		    
		  val propOfModelType = new TestProperty(modelType, ExactlyOne, "Complex Prop")
		    
		  val thingWithComplex = new SimpleTestThing("My Complex Thing",
		      propOfModelType(SimplePropertyBundle(
		        numberProp(3),
		        textProp("Text in Instance"))))
		  
		  val metaModel = new SimpleTestThing("Meta Model",
		      propOfModelType(SimplePropertyBundle(
		        numberProp(42),
		        textProp("Text from MetaModel"))), 
            interface[querki.editing.Editor].InstanceProps(propOfModelType))
		  val metaType = new ModelType(toid, metaModel.id,
		      Core.toProps(
		        Core.setName("Meta Type")))
		  registerType(metaType)
		  val metaProp = new TestProperty(metaType, QList, "Meta Property")
		  
		  val topModel = new SimpleTestThing("Top Model",
		      metaProp())
		  val metaThing1 = new TestThing("Top level Thing 1", topModel,
		      metaProp(
		        SimplePropertyBundle(propOfModelType(SimplePropertyBundle(numberProp(11), textProp("Top Text 1")))),
		        SimplePropertyBundle(propOfModelType(SimplePropertyBundle(numberProp(200), textProp("Top Text 2"))))))
		  val metaThing2 = new TestThing("Top level Thing 2", topModel,
		      metaProp(SimplePropertyBundle(propOfModelType(SimplePropertyBundle(numberProp(200), textProp("Top Text 2"))))))
		  // Note that this one intentionally doesn't include metaProp, so we can validate that the lower-level fields get stubbed:
		  val metaThing3 = new TestThing("Top level Thing 3", topModel)
      }
      implicit val s = new TSpace
      
      val result = Imexport.exportInstances(getRc, Format.CSV, s.topModel)(s.state)
      assert(result.name == "Top Model.csv")
      // The nested Model Types have InstanceProps, and the top doesn't. The result is that we get
      // the order Link Name, (Text in Model, Number in Model) -- the first two come from "Meta Property",
      // which comes after "Link Name":
      new String(result.content).stripReturns should equal("""Link Name,Text in Model,Number in Model
          |Top-level-Thing-1,Top Text 1,11
          |Top-level-Thing-2,Top Text 2,200
          |Top-level-Thing-3,Text from MetaModel,42""".stripReturns)
    }

    "work with escaped fields" in {
      class TSpace extends CommonSpace {
        val numberProp = new TestProperty(Core.IntType, ExactlyOne, "Number Prop")
        val textProp = new TestProperty(TextType, ExactlyOne, "Text Prop", Basic.DisplayNameProp("""This is "text" """))
        val myModel = new SimpleTestThing("My Model", numberProp(42), textProp(" \"Hello\" "))
        
        val instance1 = new TestThing("Instance 1", myModel)
        val instance2 = new TestThing("Instance 2", myModel, textProp("Hi, there!"))
        val instance3 = new TestThing("Instance 3", myModel, textProp("""Hello
            |there""".stripReturns))
      }
      
      implicit val s = new TSpace
      
      val result = Imexport.exportInstances(getRc, Format.CSV, s.myModel)(s.state)
      assert(result.name == "My Model.csv")
      // No Instance Properties specified, so we show the Properties in alphabetical
      // order: Name, Number Prop, Text Prop. Everything is ExactlyOne, so no empty fields:
      new String(result.content).stripReturns should equal("""Link Name,Number Prop,"This is ""text"" "
          |Instance-1,42," ""Hello"" "
          |Instance-2,42,"Hi, there!"
          |Instance-3,42,"Hello
          |there"""".stripReturns)
    }
    
    "work with simple Links" in {
      class TSpace extends CommonSpace {
        val categoryModel = new SimpleTestThing("My Category")
        val cat1 = new TestThing("Category 1", categoryModel)
        val cat2 = new TestThing("Category 2", categoryModel)
        val cat3 = new TestThing("Category 3", categoryModel)
        
        val numberProp = new TestProperty(Core.IntType, ExactlyOne, "Number Prop")
        val linkProp = new TestProperty(LinkType, ExactlyOne, "Text Prop", Links.LinkModelProp(categoryModel))
        val myModel = new SimpleTestThing("My Model", numberProp(42), linkProp(cat2))
        
        val instance1 = new TestThing("Instance 1", myModel)
        val instance2 = new TestThing("Instance 2", myModel, linkProp(cat1))
        val instance3 = new TestThing("Instance 3", myModel, linkProp(cat3))
      }
      
      implicit val s = new TSpace
      
      val result = Imexport.exportInstances(getRc, Format.CSV, s.myModel)(s.state)
      assert(result.name == "My Model.csv")
      new String(result.content).stripReturns should equal("""Link Name,Number Prop,Text Prop
          |Instance-1,42,Category 2
          |Instance-2,42,Category 1
          |Instance-3,42,Category 3""".stripReturns)
    }
    
    "work with Categories" in {
      class TSpace extends CommonSpace {
        val categoryModel = new SimpleTestThing("My Category", Links.NoCreateThroughLinkProp(true))
        val cat1 = new TestThing("Category 1", categoryModel)
        val cat2 = new TestThing("Category 2", categoryModel)
        val cat3 = new TestThing("Category 3", categoryModel)
        
        val numberProp = new TestProperty(Core.IntType, ExactlyOne, "Number Prop")
        val linkProp = new TestProperty(LinkType, QSet, "The categories", Links.LinkModelProp(categoryModel))
        val myModel = new SimpleTestThing("My Model", numberProp(42), linkProp(cat2))
        
        val instance1 = new TestThing("Instance 1", myModel)
        val instance2 = new TestThing("Instance 2", myModel, linkProp(cat1, cat3))
        val instance3 = new TestThing("Instance 3", myModel, linkProp(cat3))
      }
      
      implicit val s = new TSpace
      
      val result = Imexport.exportInstances(getRc, Format.CSV, s.myModel)(s.state)
      assert(result.name == "My Model.csv")
      // Note that the Categories are at the end because "The categories" comes after "Name" and "Number Prop":
      new String(result.content).stripReturns should equal("""Link Name,Number Prop,Category 1,Category 2,Category 3
          |Instance-1,42,,x,
          |Instance-2,42,x,,x
          |Instance-3,42,,,x""".stripReturns)
    }
  }
}