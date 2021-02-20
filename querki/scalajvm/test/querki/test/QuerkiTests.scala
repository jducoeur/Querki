package querki.test

import akka.actor.{ActorRef, Actor, ActorSystem, Props}
import akka.cluster.sharding.ShardRegion
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import models.{Thing, OID}
import querki.core.QLText
import querki.ecology._
import querki.globals.{awaitIntentionally, Ecology, QLog, EcologyInterface, EcologyMember, QuerkiEcot}
import querki.identity.User
import querki.time.{DateTime, TimeProvider}
import querki.values.{SpaceState, QLContext, RequestContext, PropAndVal}

class QuerkiTests 
  extends WordSpec
  with Matchers
  with BeforeAndAfterAll
  with EcologyMember
{
  implicit var ecology:Ecology = null
  
  QLog.runningUnitTests = true
  
  lazy val Core = interface[querki.core.Core]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val QL = interface[querki.ql.QL]
  lazy val Links = interface[querki.links.Links]
  
  lazy val ExactlyOne = Core.ExactlyOne
  lazy val Optional = Core.Optional
  lazy val QList = Core.QList
  
  class SystemEcotSemiStub(e:Ecology, aso:Option[ActorSystem]) extends querki.system.SystemEcot(e, aso, Actor.noSender) {
    override def createShardRegion(name:String, props:Props, identityExtractor:ShardRegion.ExtractEntityId, identityResolver:ShardRegion.ExtractShardId) = {
      Some(ActorRef.noSender)
    }
  }

  class ControllableTimeProvider(e: Ecology) extends QuerkiEcot(e) with TimeProvider {
    var _now: Option[DateTime] = None

    def setTime(time: DateTime): Unit = _now = Some(time)

    def now = _now.getOrElse(DateTime.now)

    def qlEndTime = new DateTime(now.getMillis + maxRunTime)

    // A short conceptual timeout for tests
    def maxRunTime: Int = 5000
  }

  /**
    * Use this to control the current time during testing.
    */
  def setTime(time: DateTime): Unit = {
    ecology.api[querki.time.TimeProvider].asInstanceOf[ControllableTimeProvider].setTime(time)
  }

  /**
   * This is the method to add the Ecots into the Ecology. By default, it creates the whole world, but
   * that is not required -- feel free to override this with a version that instantiates only some of them,
   * and stubs out others.
   */
  def createEcots(e:Ecology): Unit = {
    querki.system.SystemCreator.createTestableEcots(e)

    // Testable stubs:
    new PublicUrlStub(e)
    new SystemEcotSemiStub(e, None)
    new UserAccessStub(e)
    new ControllableTimeProvider(e)
  }
  
  def createEcology() = {
    val e = new EcologyImpl(None)
    createEcots(e)
    val state = e.init(querki.system.InitialSystemState.create(e), { (props, name) => Some(ActorRef.noSender) })
    e.api[querki.system.SystemManagement].setState(state)
    ecology = e
  }
  
  def getRcs[S <: TestSpace](state:SpaceState)(implicit space:S, requester:User = BasicTestUser):RequestContext = {
    SimpleTestRequestContext(space.owner.mainIdentity.id)
  }
  def getRc[S <: TestSpace](implicit space:S, requester:User = BasicTestUser):RequestContext = {
    getRcs(space.state)
  }
  
  /**
   * The current easiest way to declare a typical QL test. You must have declared an implicit CommonSpace or
   * descendant for this to work, but it's very boilerplate-light. Note that this supplies the Space itself
   * as the context, so you will usually need to specify explicit context at the beginning of the QL expression.
   */
  def pql[S <: TestSpace](text:String)(implicit space:S, requester:User = BasicTestUser):String = {
    pqls(text, space.state)
  }
  def pqls[S <: TestSpace](text:String, state:SpaceState)(implicit space:S, requester:User = BasicTestUser):String = {
    val rc = getRcs(state)
    val context = state.thisAsContext(rc, state, ecology)
    processQText(context, text)
  }
  def pqlt[S <: TestSpace](t:Thing, text:String)(implicit space:S, requester:User = BasicTestUser):String = {
    implicit val state = space.state
    implicit val rc = getRcs(state)
    val context = t.thisAsContext
    processQText(context, text, Some(t))
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
    import scala.concurrent._
    import scala.concurrent.duration._
    
    val qt = QLText(text)
    val wikitext = Await.result(QL.process(qt, context, None, lexicalOpt), 1 second)
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
    val rc = SimpleTestRequestContext(space.owner.mainIdentity.id)
    thing.thisAsContext(rc, state, ecology)
  }
  
  def commonThingAsContext(f: CommonSpace => Thing)(implicit requester:User = BasicTestUser):QLContext = thingAsContext(commonSpace, f)
  
  /**
   * This is a variant of thingAsContext, intended for use when we have "saved" and "loaded" the state, so we
   * aren't directly using a derivative of CommonSpace.
   */
  def loadedContext(state:SpaceState, id:OID)(implicit requester:User = BasicTestUser):QLContext = {
    val thing = state.anything(id).get
    val rc = SimpleTestRequestContext(state.owner)
    thing.thisAsContext(rc, state, ecology)
  }
  
  /**
   * The standard rendering for a single Link that makes its way to the output.
   */
  def linkText(t:Thing):String = {
    // This is a bit convoluted, but is what we actually display in the link text. Indeed, it's
    // arguably insufficient -- we should be factoring nameOrComputed into here.
    def fullLookupDisplayName:Option[PropAndVal[_]] = {
      val dispOpt = t.localProp(DisplayNameProp)
      if (dispOpt.isEmpty || dispOpt.get.isEmpty)
        t.localProp(Core.NameProp)
      else
        dispOpt
    }

    val display = awaitIntentionally(fullLookupDisplayName.get.renderPlain).raw
    val name = t.canonicalName.map(querki.core.NameUtils.toUrl(_)).getOrElse(display)
    "[" + display + "](" + name + ")"     
  }
  
  def listOf(strs: String*): String = {
    strs.map { str => s"\n$str" }.mkString
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
  
  def listOfLinkText(ids:OID*)(implicit state:SpaceState):String = {
    val things = ids.map(state.anything(_).get)
    listOfLinkText(things:_*)
  }
  
  def oneTag(tag:String):String = {
    s"[$tag](${tag.replace(" ", "+")})"
  }
  
  def listOfTags(tags:String*):String = {
    val lines = tags.map(tag => s"\n[$tag](${tag.replace(" ", "+")})")
    lines.mkString
  }
  
  def expectedWarning(warningName:String):String = s"{{_warning:$warningName}}"
  
  // This is the non-error display of a Tag literal that is unknown
  // TODO: this *should* just reuse oneTag() -- but it can't, because the URL from unknownName (in
  // QLEcot.UnknownNameType) doesn't match the one for Tags (in TagsEcot.TagThing)! These really
  // should be reconciled...
  def unknownName(name:String):String = s"{{_unknownName:[$name](${name.replace(" ", "-")})}}"
  
  // Commonly used Ecots and pieces therein:
  lazy val DisplayNameProp = interface[querki.basic.Basic].DisplayNameProp

  // Turns on massive QLParser spewage. Should not be checked in!
  def turnOnContextLogging() = querki.util.Config.test("querki.test.logContexts", "true")
}
