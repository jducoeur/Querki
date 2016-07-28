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
 * This is the stand-in for an actual Property, from outside the black box -- this is used to make
 * the Property useful in tests.
 */
case class PropRecord[VT, RT](oid:OID, cType:Collection, pType:PType[VT] with PTypeBuilder[VT, RT]) {
  def apply(raws:RT*) = (oid, QValue.make(cType, pType, raws:_*))
}

/**
 * This is a specialized version of TestSpace that creates a SpaceCore and wraps around that. It allows a
 * much richer set of interactions than the traditional CommonSpace, but requires some setup effort.
 */
class SpaceCoreSpace(implicit val ecology:Ecology) extends TestSpace {
  val world = new TestWorld
  
  val sc = new TestSpaceCore(toid(), this)
  
  override lazy val state = sc.state
  
  def !(msg:AnyRef) = sc.aroundReceive(msg)
}

/**
 * This is the SpaceCore-based equivalent of CommonSpace. It has essentially the same elements, but
 * builds them dynamically instead of simply declaring them.
 */
class CommonCoreSpace(implicit e:Ecology) extends SpaceCoreSpace()(e) {
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Links = interface[querki.links.Links]
  lazy val Tags = interface[querki.tags.Tags]
  
  lazy val ExternalLinkType = Links.URLType
  lazy val TextType = Core.TextType
  lazy val LinkType = Core.LinkType
  lazy val TagType = Tags.NewTagSetType
  
  def addSomething(name:String, kind:Kind, model:OID, propList:(OID, QValue)*):OID = {
    val props = makePropFetcher(name, propList)
    val Some(ThingFound(oid, _)) = this ! CreateThing(owner, sc.id, kind, model, props)
    oid
  }
  
  def addProperty[VT, RT](name:String, coll:Collection, tpe:PType[VT] with PTypeBuilder[VT, RT]):PropRecord[VT, RT] = {
    val oid = addSomething(name, Kind.Property, UrPropOID,
      Core.CollectionProp(coll),
      Core.TypeProp(tpe)
    )
    PropRecord(oid, coll, tpe)
  }
  
  def addThing(name:String, model:OID, propList:(OID, QValue)*) = {
    addSomething(name, Kind.Thing, model, propList:_*)
  }
  
  def addSimpleThing(name:String, propList:(OID, QValue)*) = {
    addThing(name, SimpleThingOID, propList:_*)
  }
  
  def addSimpleModel(name:String, propList:(OID, QValue)*) = {
    addThing(name, SimpleThingOID,
      (Core.IsModelProp(true) +: propList):_*
    )
  }
  
  // Boot the Space up
  this ! InitialState(owner, sc.id, "Test Space", owner.mainIdentity.id)
  
  // Build the contents of CommonSpace in it
  // First, create all the Properties...
  addProperty("Single Link", ExactlyOne, LinkType)
  addProperty("Optional Link", Optional, LinkType)
  addProperty("My List of Links", QList, LinkType)
  addProperty("My Set of Links", QSet, LinkType)
  
  addProperty("Single Tag", ExactlyOne, TagType)
  addProperty("Optional Tag", Optional, TagType)
  addProperty("My List of Tags", QList, TagType)
  addProperty("My Set of Tags", QSet, TagType)
  
  addProperty("My List of URLs", QList, ExternalLinkType)
  addProperty("My Optional URL", Optional, ExternalLinkType)
  
  addProperty("Single Text", ExactlyOne, TextType)
  val optTextProp = addProperty("My Optional Text", Optional, TextType)
  
  // Then the Models and Things...
  val testModelId = addSimpleModel("My Model")
  val instance = addThing("My Instance", testModelId, optTextProp("Hello world"))
  val withDisplayName = addSimpleThing("Interesting Display Name", Basic.DisplayNameProp("""My name is "interesting"!"""))
  val trivialThing = addSimpleThing("Trivial")  
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
  
  "CommonCoreSpace" should {  
    "pass some common tests" in {
      implicit val s = new CommonCoreSpace

      pql("[[My Instance -> My Optional Text]]") should equal ("Hello world")
      
      pql("[[My Model -> _kind]]") should equal ("0")
      pql("[[Text Type -> _kind]]") should equal ("1")
      pql("[[My List of Links._self -> _kind]]") should equal ("2")
      pql("[[Test Space -> _kind]]") should equal ("3")
      pql("[[Optional -> _kind]]") should equal ("4")
      
      pql("""[[My Instance -> _model -> _oid]]""") should equal(s.testModelId.toThingId.toString)
      pql("""[[My Model -> _model -> _oid]]""") should equal(SimpleThingOID.toThingId.toString)
    }
  }
}
