package querki.test

import org.scalatest.{WordSpec, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers

import models.{Thing}
import models.system.{NameType, QLText}

import ql.QLParser

import querki.ecology._

import querki.identity.User

import querki.values.{QLContext, SpaceState}

class QuerkiTests 
  extends WordSpec
  with ShouldMatchers
  with BeforeAndAfterAll
{
  // TODO: FIX THIS!!!
  lazy val ecology:Ecology = querki.ecology.Ecology
  
  // Just for efficiency, we create the CommonSpace once -- it is immutable, and good enough for
  // most purposes:
  lazy val commonSpace = new CommonSpace
  override def beforeAll() = { 
    commonSpace
  }
  def commonState = commonSpace.state
  
  def processQText(context:QLContext, text:String):String = {
    val qt = QLText(text)
    val parser = new QLParser(qt, context)
    val wikitext = parser.process
    wikitext.plaintext
  }
  
  def spaceAndThing[S <: CommonSpace](space:S, f: S => Thing):(SpaceState, Thing) = {
    val thing = f(space)
    (space.state, thing)
  }
  
  /**
   * Note that, by default, requests are made by the BasicTestUser, who has nothing to do with this Space.
   * To make the request in the name of the owner or a member instead, set an implicit User in the test.
   */
  def thingAsContext[S <: CommonSpace](space:S, f: S => Thing)(implicit requester:User = BasicTestUser):QLContext = {
    val (state, thing) = spaceAndThing(space, f)
    val rc = SimpleTestRequestContext(space.owner.mainIdentity.id, state, thing, ecology)
    thing.thisAsContext(rc)
  }
  
  def commonThingAsContext(f: CommonSpace => Thing)(implicit requester:User = BasicTestUser):QLContext = thingAsContext(commonSpace, f)
  
  /**
   * Given a list of expected Things that comes out at the end of a QL expression, this is the
   * wikitext for their rendered Links.
   */
  def listOfLinkText(things:Thing*):String = {
    val lines = things.map { t =>
      val display = t.displayName
      val name = t.canonicalName.map(NameType.toUrl(_)).getOrElse(display)
      "\n[" + display + "](" + name + ")" 
    }
    lines.mkString
  }
  
  def expectedWarning(warningName:String):String = s"{{_warning:$warningName}}"
  
  // Commonly used Ecots and pieces therein:
  lazy val DisplayNameProp = getInterface[querki.basic.Basic].DisplayNameProp
}
