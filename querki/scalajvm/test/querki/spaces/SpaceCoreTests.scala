package querki.spaces

import akka.actor.Actor.noSender

import models._
import Kind.Kind
import Thing.{emptyProps, PropMap}
import querki.basic.MOIDs.SimpleThingOID
import querki.core.MOIDs.UrPropOID
import querki.globals._
import querki.identity.User
import querki.persistence._
import querki.spaces.messages._
import querki.test._
import querki.values.QValue
  
case class HistoryRecord(sequenceNr:Long, msg:UseKryo)
  
/**
 * For testing, we use a version of SpaceCore built around the synchronous TCIdentity.
 */
class TestSpaceCore(val id:OID, testSpace:TestSpace, initHistory:List[HistoryRecord] = List.empty)(implicit e:Ecology) extends SpaceCore[TCIdentity](TestRTCAble) {
  
  /**
   * This is the "history" of "persisted" events, in reverse chronological order. (That is, most recent is
   * at the front.)
   */
  var history = initHistory
  
  def doPersist[A <: UseKryo](event:A)(handler: (A) => Unit) = {
    history = HistoryRecord(lastSequenceNr, event) :: history
    lastSequenceNr += 1
    handler(event)
  }
  
  var lastSequenceNr:Long = 0
  
  /**
   * This sends the given message back to sender.
   */
  def respond(msg:AnyRef) = {
    currentResponses = msg :: currentResponses
  }
  
  /**
   * The responses to the current message.
   */
  var currentResponses:List[AnyRef] = List.empty
  
  /**
   * Called by the test code. Returns the most recent response, if there were any.
   */
  def aroundReceive(msg:AnyRef):Option[AnyRef] = {
    currentResponses = List.empty
    receiveCommand(msg)
    currentResponses.headOption
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
  
  /**
   * If an initial history was provided, that's effectively the persistence log, so play it
   * before we do anything else.
   */
  if (!initHistory.isEmpty) {
    // Reverse it to get chrono order:
    val playHistory = initHistory.reverse
    playHistory.foreach { receiveRecover(_) }
  }
}

/**
 * This is a specialized version of TestSpace that creates a SpaceCore and wraps around that. It allows a
 * much richer set of interactions than the traditional CommonSpace, but requires some setup effort.
 */
class SpaceCoreSpace(implicit val ecology:Ecology) extends TestSpace {
  val world = new TestWorld
  
  val sc = new TestSpaceCore(toid(), this)
  
  override def state = sc.state
  
  def !(msg:AnyRef) = sc.aroundReceive(msg)
}

class SpaceCoreTests extends QuerkiTests {  
  "SpaceCoreSpace" should {
    "throw an exception if it doesn't start with InitialState" in {
      implicit val s = new SpaceCoreSpace

      intercept[Exception] {
        s ! CreateThing(s.owner, s.sc.id, Kind.Thing, SimpleThingOID, emptyProps)
      }
    }
  }
}
