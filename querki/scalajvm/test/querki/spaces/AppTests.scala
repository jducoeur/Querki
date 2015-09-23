package querki.spaces

import models.Thing

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
    }
    
    val mid1 = new TestApp {
      override val apps = Seq(highest)
      
      val duplicateThing = new SimpleTestThing("Duplicate Thing")
    }
    
    val mid2 = new TestApp {
      val mySimplePage = new SimpleTestThing("Simple Page")
    }
    
    val mid3 = new TestApp {
      override val apps = Seq(highest)
      
      val duplicateThing = new SimpleTestThing("Duplicate Thing")
    }
    
    val main = new TestApp {
      override val apps = Seq(mid1, mid2, mid3)
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
  }
}
