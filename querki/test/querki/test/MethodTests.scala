package querki.test

import org.scalatest.{WordSpec, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers

import models.{Thing, ThingState, Wikitext}
import models.Thing._
import models.system.{QLText}
import models.system.OIDs.{PageOID}

import ql.QLParser

import querki.values.{QLContext, SpaceState}

class CommonMethodSpace extends TestSpace {
  // In the simple case, we only have one Space, so it can own the World:
  val world = new TestWorld
  
  /***********************************************
   * THINGS
   ***********************************************/

  /**
   * The generic "sandbox" Thing, which serves as a useful default if you don't
   * care much about the details.
   */
  lazy val sandbox = ThingState(toid(), spaceId, PageOID, 
    toProps(
      setName("Sandbox")))
        
  override lazy val things = Seq(
    sandbox
  )
}

class MethodTests 
  extends WordSpec
  with ShouldMatchers
  with BeforeAndAfterAll
{
  /***********************************************
   * COMMON METHODS
   ***********************************************/
  
  override def beforeAll() = { }
  
  def normalSpaceState = new CommonMethodSpace().state
  
  def processQText(context:QLContext, text:String):String = {
    val qt = QLText(text)
    val parser = new QLParser(qt, context)
    val wikitext = parser.process
    wikitext.plaintext
  }
  
  def commonSpaceAndThing(f: CommonMethodSpace => Thing):(SpaceState, Thing) = {
    val space = new CommonMethodSpace
    val thing = f(space)
    (space.state, thing)
  }
  
  def commonThingAsContext(f: CommonMethodSpace => Thing):QLContext = {
    val (state, thing) = commonSpaceAndThing(f)
    val rc = SimpleTestRequestContext(state, thing)
    thing.thisAsContext(rc)
  }
  
  /***********************************************
   * TESTS
   ***********************************************/
  
  "_linkButton" should {
    "work with a Link to Thing" in {
      processQText(commonThingAsContext(_.sandbox), """[[_linkButton(""hello"")]]""") should 
        equal ("""<a class="btn btn-primary" href="Sandbox">hello</a>""")
    }
  }
}