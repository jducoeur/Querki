package querki.spaces

import akka.actor.Actor.noSender

import models._
import Kind.Kind
import Thing.{emptyProps, PropMap}
import querki.basic.MOIDs.SimpleThingOID
import querki.globals._
import querki.identity.User
import querki.persistence._
import querki.spaces.messages._
import querki.test._
  
/**
 * For testing, we use a version of SpaceCore built around the synchronous TCIdentity.
 */
class TestSpaceCore(val id:OID, testSpace:TestSpace)(implicit e:Ecology) extends SpaceCore[TCIdentity](TestRTCAble) {
  def doPersist[A <: UseKryo](event:A)(handler: (A) => Unit) = {
    // TODO: record these persists so that we can replay them in later tests.
    lastSequenceNr += 1
  }
  
  var lastSequenceNr:Long = 0
  
  /**
   * This sends the given message back to sender.
   */
  def respond(msg:AnyRef) = {
    // TODO: provide a hook to check these responses
  }
  
  /**
   * Give hooks an opportunity to chime in on this change.
   * 
   * TODO: can we do this for real, letting the rest of the Ecology play? Seems potentially problematic
   * in the synchronous unit tests, but I'm intrigued.
   */
  def offerChanges(who:User, modelId:Option[OID], thingOpt:Option[Thing], kind:Kind, propsIn:PropMap, changed:Seq[OID]):TCIdentity[ThingChangeRequest] = {
    // The null below is ugly, but I don't have an easy way to provide a Requester reference. How *should* we do this
    // in tests? We basically want to carry the TCIdentity abstraction further along.
    val tcr = ThingChangeRequest(who, null, state, noSender, modelId, None, kind, propsIn, changed)
    TestRTCAble.successful(tcr)
  }
  
  def allocThingId():TCIdentity[OID] = {
    TestRTCAble.successful(testSpace.toid())
  }
  
  def notifyUpdateState() = {
    // TODO: hook and test this?
  }
  
  def changeSpaceName(newName:String, newDisplay:String) = {}
}

/**
 * This is a specialized version of TestSpace that creates a SpaceCore and wraps around that. It allows a
 * much richer set of interactions than the traditional CommonSpace, but requires some setup effort.
 */
class SpaceCoreSpace(implicit val ecology:Ecology) extends TestSpace {
  val world = new TestWorld
  
  val sc = new TestSpaceCore(toid(), this)
  
  override lazy val state = sc.state
  
  def !(msg:Any) = sc.receiveCommand(msg)
}

class SpaceCoreTests extends QuerkiTests {
  "SpaceCore" should {
    "throw an exception if it doesn't start with InitialState" in {
      implicit val s = new SpaceCoreSpace

      intercept[Exception] {
        s ! CreateThing(s.owner, s.sc.id, Kind.Thing, SimpleThingOID, emptyProps)
      }
    }
    
    "work in a complex scenario" in {
      implicit val s = new SpaceCoreSpace
      
      s ! InitialState(s.owner, s.sc.id, "Test SpaceCore", s.owner.mainIdentity.id)
    }
  }
}