package querki.spaces

import models.Thing
import Thing._

import querki.globals._
import querki.test._

/**
 * Complex structural tests for Apps.
 * 
 * @author jducoeur
 */
class AppTests extends QuerkiTests {
  def getEcology = ecology
  
  /**
   * A wrapper that describes our standard complex App-based test world. Test code usually
   * extends this class, and puts the test code in the extension.
   */
  class WorldTest extends TestWorld {
    val theWorld = this
    
    trait TestApp extends TestSpace {
      val world = theWorld
      def ecology = getEcology
    }
  
    val highest = new TestApp {
      val highestThing = new SimpleTestThing("Highest Thing")
      
      val rootModel = new SimpleTestThing("My Root Model", Core.IsModelProp(true))
      
      val myNumProp = new TestProperty(Core.IntType, QList, "My Nums")
      
      val highestInstance = new TestThing("Highest Instance", rootModel)
    }
    
    val mid1 = new TestApp {
      override val apps = Seq(highest)
      
      val duplicateThing = new SimpleTestThing("Duplicate Thing")
      val plusOne = new SimpleTestThing("Plus One", Basic.ApplyMethod("_plus(1)"))
    }
    
    val mid2 = new TestApp {
      val mySimplePage = new SimpleTestThing("Simple Page")
      val firstTwo = new SimpleTestThing("First Two", Basic.ApplyMethod("_take(2)"))
      
      val mid2Instance = new TestThing("Mid2 Instance", highest.rootModel)
    }
    
    val mid3 = new TestApp {
      override val apps = Seq(highest)
      
      val duplicateThing = new SimpleTestThing("Duplicate Thing")
      val numThing = new SimpleTestThing("Num Thing", highest.myNumProp(4, 6, 8))
    }
    
    val main = new TestApp {
      override val apps = Seq(mid1, mid2, mid3)
      
      val mainInstance = new TestThing("Main Instance", highest.rootModel)
    }
    
    implicit val s = main
    
    // Check that the specified name resolves to the specified Thing
    def testName(name:String, expected:Thing) = {
      val tOpt = s.state.anythingByName(name)
      assert(tOpt.isDefined)
      assert(tOpt.get.id == expected.id)      
    }
  }
  
  "A space with complex apps" should {
    "fetch from System if not otherwise defined" in {
      new WorldTest {
        testName("Simple Thing", Basic.SimpleThing)
      }
    }
    
    "fetch from the Highest App" in {
      new WorldTest {
        testName("Highest Thing", highest.highestThing)
      }
    }
    
    "override System" in {
      new WorldTest {
        testName("Simple-Page", mid2.mySimplePage)
      }
    }
    
    "override in order" in {
      new WorldTest {
        testName("Duplicate Thing", mid1.duplicateThing)
      }
    }
    
    "combine elements from various apps" in {
      new WorldTest {
        pql("[[Num Thing -> My Nums -> Plus One -> First Two -> _commas]]") should
          equal("5, 7")
      }
    }
    
    // This is indirectly testing SpaceState.accumulateAll, checking that it doesn't accumulate duplicates.
    // In the initial naive implementation, it fails because there are two routes to highest:
    "not have duplicates" in {
      new WorldTest {
        val allModels = s.state.allModels
        val targetModel = allModels.filter(_.id == highest.rootModel.id)
        assert(targetModel.size == 1)
      }
    }
  }
  
  "_instances" should {
    "includes Instances from both Apps and children" in {
      new WorldTest {
        pql("[[My Root Model._instances -> _sort]]") should
          equal(listOfLinkText(highest.highestInstance, main.mainInstance, mid2.mid2Instance))
      }
    }
  }
}
