package querki.test

import org.scalatest.{WordSpec, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers

import models.{Thing}

import querki.core.QLText

import querki.ecology._

import querki.identity.User

import querki.values.{QLContext, SpaceState}

class QuerkiTests 
  extends WordSpec
  with ShouldMatchers
  with BeforeAndAfterAll
  with EcologyMember
{
  implicit var ecology:Ecology = null
  
  lazy val Core = interface[querki.core.Core]
  lazy val QL = interface[querki.ql.QL]
  
  lazy val ExactlyOne = Core.ExactlyOne
  lazy val Optional = Core.Optional
  lazy val QList = Core.QList
  
  /**
   * This is the method to add the Ecots into the Ecology. By default, it creates the whole world, but
   * that is not required -- feel free to override this with a version that instantiates only some of them,
   * and stubs out others.
   */
  def createEcots(e:Ecology) = {
    querki.system.SystemCreator.createAllEcots(e)
  }
  
  def createEcology() = {
    val e = new EcologyImpl
    createEcots(e)
    val state = e.init(querki.system.InitialSystemState.create(e))
    e.api[querki.system.SystemManagement].setState(state)
    ecology = e
  }
  
  // Just for efficiency, we create the CommonSpace once -- it is immutable, and good enough for
  // most purposes:
  var commonSpace:CommonSpace = null
  override def beforeAll() = {
    createEcology
    commonSpace = new CommonSpace
  }
  def commonState = commonSpace.state
  
  def processQText(context:QLContext, text:String):String = {
    val qt = QLText(text)
    val wikitext = QL.process(qt, context)
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
      val name = t.canonicalName.map(querki.core.NameUtils.toUrl(_)).getOrElse(display)
      "\n[" + display + "](" + name + ")" 
    }
    lines.mkString
  }
  
  def expectedWarning(warningName:String):String = s"{{_warning:$warningName}}"
  
  // Commonly used Ecots and pieces therein:
  lazy val DisplayNameProp = interface[querki.basic.Basic].DisplayNameProp
}
