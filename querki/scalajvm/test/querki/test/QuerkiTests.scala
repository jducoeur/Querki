package querki.test

import org.scalatest.{WordSpec, BeforeAndAfterAll, Matchers}

import models.{OID, Thing}

import querki.core.QLText

import querki.ecology._

import querki.identity.User

import querki.values.{QLContext, RequestContext, SpaceState}

class QuerkiTests 
  extends WordSpec
  with Matchers
  with BeforeAndAfterAll
  with EcologyMember
{
  implicit var ecology:Ecology = null
  
  lazy val Core = interface[querki.core.Core]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val QL = interface[querki.ql.QL]
  lazy val Links = interface[querki.links.Links]
  
  lazy val ExactlyOne = Core.ExactlyOne
  lazy val Optional = Core.Optional
  lazy val QList = Core.QList
  
  /**
   * This is the method to add the Ecots into the Ecology. By default, it creates the whole world, but
   * that is not required -- feel free to override this with a version that instantiates only some of them,
   * and stubs out others.
   */
  def createEcots(e:Ecology) = {
    querki.system.SystemCreator.createTestableEcots(e, None)

    // Testable stubs:
    new UserAccessStub(e)
    new PublicUrlStub(e)
  }
  
  def createEcology() = {
    val e = new EcologyImpl
    createEcots(e)
    val state = e.init(querki.system.InitialSystemState.create(e), { (props, name) => None })
    e.api[querki.system.SystemManagement].setState(state)
    ecology = e
  }
  
  def getRcs[S <: CommonSpace](state:SpaceState)(implicit space:S, requester:User = BasicTestUser):RequestContext = {
    SimpleTestRequestContext(space.owner.mainIdentity.id, ecology)
  }
  def getRc[S <: CommonSpace](implicit space:S, requester:User = BasicTestUser):RequestContext = {
    getRcs(space.state)
  }
  
  /**
   * The current easiest way to declare a typical QL test. You must have declared an implicit CommonSpace or
   * descendant for this to work, but it's very boilerplate-light. Note that this supplies the Space itself
   * as the context, so you will usually need to specify explicit context at the beginning of the QL expression.
   */
  def pql[S <: CommonSpace](text:String)(implicit space:S, requester:User = BasicTestUser):String = {
    pqls(text, space.state)
  }
  def pqls[S <: CommonSpace](text:String, state:SpaceState)(implicit space:S, requester:User = BasicTestUser):String = {
    val rc = getRcs(state)
    val context = state.thisAsContext(rc, state)
    processQText(context, text)
  }

  implicit class testableString(str:String) {
    // Multi-line test strings should use this, to deal with Unix vs. Windows problems:
    def stripReturns:String = str.replace("\r", "").stripMargin
    
    def trimStart(str:String):String = {
      str.indexWhere(c => c != ' ' && c != '\t') match {
        case -1 => ""
        case n => str.substring(n)
      }
    }
    
    // Similar to stripReturns, but instead of using stripMargin, we trim the front of the line.
    def strip:String = str.replace("\r", "").lines.map(trimStart(_)).mkString("\n")
  }
    
  // Just for efficiency, we create the CommonSpace once -- it is immutable, and good enough for
  // most purposes:
  var commonSpace:CommonSpace = null
  override def beforeAll() = {
    createEcology
    commonSpace = new CommonSpace
  }
  def commonState = commonSpace.state
  
  def processQText(context:QLContext, text:String, lexicalOpt:Option[Thing] = None):String = {
    val qt = QLText(text)
    val wikitext = QL.process(qt, context, None, lexicalOpt)
    wikitext.plaintext.stripReturns
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
    val rc = SimpleTestRequestContext(space.owner.mainIdentity.id, ecology)
    thing.thisAsContext(rc, state)
  }
  
  def commonThingAsContext(f: CommonSpace => Thing)(implicit requester:User = BasicTestUser):QLContext = thingAsContext(commonSpace, f)
  
  /**
   * This is a variant of thingAsContext, intended for use when we have "saved" and "loaded" the state, so we
   * aren't directly using a derivative of CommonSpace.
   */
  def loadedContext(state:SpaceState, id:OID)(implicit requester:User = BasicTestUser):QLContext = {
    val thing = state.anything(id).get
    val rc = SimpleTestRequestContext(state.owner, ecology)
    thing.thisAsContext(rc, state)
  }
  
  /**
   * The standard rendering for a single Link that makes its way to the output.
   */
  def linkText(t:Thing):String = {
    val display = t.displayName
    val name = t.canonicalName.map(querki.core.NameUtils.toUrl(_)).getOrElse(display)
    "[" + display + "](" + name + ")"     
  }
  
  /**
   * Given a list of expected Things that comes out at the end of a QL expression, this is the
   * wikitext for their rendered Links. Note that this is *specifically* for a List or Set.
   * If you have ExactlyOne or Optional, use linkText() instead -- the rendering is slightly
   * different.
   */
  def listOfLinkText(things:Thing*):String = {
    val lines = things.map { t =>
      "\n" + linkText(t)
    }
    lines.mkString
  }
  
  def listOfTags(tags:String*):String = {
    val lines = tags.map(tag => s"\n[$tag](${tag.replace(" ", "+")})")
    lines.mkString
  }
  
  def expectedWarning(warningName:String):String = s"{{_warning:$warningName}}"
  
  // Commonly used Ecots and pieces therein:
  lazy val DisplayNameProp = interface[querki.basic.Basic].DisplayNameProp

  // Turns on massive QLParser spewage. Should not be checked in!
  def turnOnContextLogging() = querki.util.Config.test("querki.test.logContexts", "true")
}
