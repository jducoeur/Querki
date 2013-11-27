package querki.test

import org.scalatest.{WordSpec, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers

import models.{Thing, ThingState, Wikitext}
import models.Thing._
import models.system.{ExactlyOne, Optional, QList, QSet}
import Optional.QNone
import models.system.{ExternalLinkType, LinkType, QLText}
import models.system.OIDs.{PageOID}

import ql.QLParser

import querki.values.{QLContext, SpaceState}

class CommonMethodSpace extends TestSpace {
  // In the simple case, we only have one Space, so it can own the World:
  val world = new TestWorld
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val listLinksProp = new TestProperty(LinkType, QList, "My List of Links")
  lazy val listURLProp = new TestProperty(ExternalLinkType, QList, "My List of URLs")
  
  lazy val optURLProp = new TestProperty(ExternalLinkType, Optional, "My Optional URL")
  
  override lazy val props = Seq(
    listLinksProp,
    listURLProp,
    optURLProp
  )

  /***********************************************
   * THINGS
   ***********************************************/

  /**
   * The generic "sandbox" Thing, which serves as a useful default if you don't
   * care much about the details.
   */
  val sandbox = new SimpleTestThing("Sandbox")
     
  lazy val withUrlOID = toid()
  val withUrl = new TestThing(withUrlOID, "With URL",
      optURLProp("http://www.google.com/"),
      listURLProp("http://www.google.com/", "http://www.querki.net/"))
  
  val withoutUrl = new SimpleTestThing("Without URL", optURLProp())
}

class MethodTests 
  extends WordSpec
  with ShouldMatchers
  with BeforeAndAfterAll
{
  /***********************************************
   * COMMON METHODS
   ***********************************************/

  // Just for efficiency, we create the CommonSpace once -- it is immutable, and good enough for
  // most purposes:
  var _commonSpace:CommonMethodSpace = null
  override def beforeAll() = { 
    _commonSpace = new CommonMethodSpace
  }
  
  def processQText(context:QLContext, text:String):String = {
    val qt = QLText(text)
    val parser = new QLParser(qt, context)
    val wikitext = parser.process
    wikitext.plaintext
  }
  
  def spaceAndThing[S <: CommonMethodSpace](space:S, f: S => Thing):(SpaceState, Thing) = {
    val thing = f(space)
    (space.state, thing)
  }
  
  def thingAsContext[S <: CommonMethodSpace](space:S, f: S => Thing):QLContext = {
    val (state, thing) = spaceAndThing(space, f)
    val rc = SimpleTestRequestContext(state, thing)
    thing.thisAsContext(rc)
  }
  
  def commonThingAsContext(f: CommonMethodSpace => Thing):QLContext = thingAsContext(_commonSpace, f)
  
  /***********************************************
   * TESTS
   ***********************************************/
  
  // === _linkButton ===
  "_linkButton" should {
    "work with a Link to Thing" in {
      processQText(commonThingAsContext(_.sandbox), """[[_linkButton(""hello"")]]""") should 
        equal ("""<a class="btn btn-primary" href="Sandbox">hello</a>""")
    }
    "work with an external URL" in {
      processQText(commonThingAsContext(_.withUrl), """[[My Optional URL -> _linkButton(""hello"")]]""") should
        equal ("""<a class="btn btn-primary" href="http://www.google.com/">hello</a>""")
    }
    "quietly ignore an empty context" in {
      processQText(commonThingAsContext(_.withoutUrl), """[[My Optional URL -> _linkButton(""hello"")]]""") should
        equal ("")      
    }
  }
  
  // === _showLink ===
  "_showLink" should {
    "work with a Link to Thing" in {
      processQText(commonThingAsContext(_.sandbox), """[[_showLink(""hello"")]]""") should 
        equal ("""[hello](Sandbox)""")
    }
    "work with an external URL" in {
      processQText(commonThingAsContext(_.withUrl), """[[My Optional URL -> _showLink(""hello"")]]""") should
        equal ("""[hello](http://www.google.com/)""")
    }
    "quietly ignore an empty context" in {
      processQText(commonThingAsContext(_.withoutUrl), """[[My Optional URL -> _showLink(""hello"")]]""") should
        equal ("")      
    }
    "work with a list of external URLs" in {
      // Note that a List result will have newlines in the QText, intentionally:
      processQText(commonThingAsContext(_.withUrl), """[[My List of URLs -> _showLink(""hello"")]]""") should
        equal ("\n[hello](http://www.google.com/)\n[hello](http://www.querki.net/)")      
    }
    "work with a list of Links" in {
      class testSpace extends CommonMethodSpace {
        val withLinks = new SimpleTestThing("With Links", listLinksProp(sandbox.id, withUrlOID))
      }
      
      processQText(thingAsContext[testSpace](new testSpace, _.withLinks), """[[My List of Links -> _showLink(Name)]]""") should
        equal ("\n[Sandbox](Sandbox)\n[With URL](With-URL)")
    }
  }
}