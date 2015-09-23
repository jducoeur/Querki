package querki.spaces

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
   * The world that underlies all of these Spaces.
   */
  implicit val testWorld = new TestWorld
  
  trait TestApp extends TestSpace {
    val world = testWorld
    def ecology = getEcology
  }
  
  class HighestApp extends TestApp {
    val highestThing = new SimpleTestThing("Highest Thing")
  }
  
  class Middle1(override val apps:TestSpace*) extends TestApp {
  }
  
  class Middle2(override val apps:TestSpace*) extends TestApp {
  }
  
  class Middle3(override val apps:TestSpace*) extends TestApp {
  }
  
  class TSpace(override val apps:TestSpace*) extends TestApp {
  }
  
  class WithSpaces {
    val highest = new HighestApp
    val mid1 = new Middle1(highest)
    val mid2 = new Middle2()
    val mid3 = new Middle3(highest)
    val main = new TSpace(mid1, mid2, mid3)
    
    implicit val s = main
  }
  
  "A space with complex apps" should {
    "fetch from System if not otherwise defined" in {
      new WithSpaces {
        val tOpt = s.state.anythingByName("Simple Thing")
        assert(tOpt.isDefined)
        assert(tOpt.get.id == Basic.SimpleThing.id)
      }
    }
    
    "fetch from the Highest App" in {
      new WithSpaces {
        val tOpt = s.state.anythingByName("Highest Thing")
        assert(tOpt.isDefined)
        assert(tOpt.get.id == highest.highestThing.id)
      }
    }
  }
}
