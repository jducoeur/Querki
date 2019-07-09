package querki.spaces

import akka.actor.Actor.{noSender, Receive}
import akka.persistence._

import models._
import Kind.Kind
import querki.basic.MOIDs.SimpleThingOID
import querki.conversations.TestConversations
import querki.core.MOIDs.{UrPropOID, UrTypeOID}
import querki.globals._
import querki.identity.{Identity, PublicIdentity, User}
import querki.persistence._
import querki.publication._
import querki.spaces.SpaceMessagePersistence.SpaceEvent
import querki.spaces.messages._
import querki.test._
import querki.time._
import querki.values.{QValue, SpaceVersion}
  
case class TestSpaceConfig(snapshotInterval:Option[Int])
  
/**
 * For testing, we use a version of SpaceCore built around the synchronous TCIdentity.
 */
class TestSpaceCore(
  val id:OID, 
  testSpace:TestSpace, 
  val config:Option[TestSpaceConfig] = None, 
  val initHistory:List[HistoryRecord] = List.empty)(implicit e:Ecology) 
  extends SpaceCore[TCIdentity](TestRTCAble) with PersistentCoreTestBase
{
  /**
   * Give hooks an opportunity to chime in on this change.
   * 
   * TODO: can we do this for real, letting the rest of the Ecology play? Seems potentially problematic
   * in the synchronous unit tests, but I'm intrigued.
   */
  def offerChanges(who:User, modelId:Option[OID], thingOpt:Option[Thing], kind:Kind, propsIn:PropMap, changed:Seq[OID])(state:SpaceState):TCIdentity[ThingChangeRequest] = {
    // The null below is ugly, but I don't have an easy way to provide a Requester reference. How *should* we do this
    // in tests? We basically want to carry the TCIdentity abstraction further along.
    val tcr = ThingChangeRequest(who, null, state, noSender, modelId, None, kind, propsIn, changed)
    TestRTCAble.successful(tcr)
  }
  
  def allocThingId():TCIdentity[OID] = {
    TestRTCAble.successful(testSpace.toid())
  }
  
  def allocThingIds(nIds:Int):TCIdentity[Seq[OID]] = {
    TestRTCAble.successful(testSpace.world.oidBlock(nIds))
  }
  
  def notifyUpdateState(events: Option[List[SpaceEvent]]) = {
    // TODO: hook and test this?
  }
  
  def changeSpaceName(newName:String, newDisplay:String) = {}
    
  /**
   * Allow calling tests to override the Snapshot frequency, to ensure snapshots:
   */
  override def getSnapshotInterval = config.flatMap(_.snapshotInterval).getOrElse(100)

  /**
   * We don't currently expect this to be called in the test environment.
   */
  def recoverOldSpace():TCIdentity[Option[SpaceState]] = TestRTCAble.successful(None)
  
  def fetchOwnerIdentity(ownerId:OID):TCIdentity[PublicIdentity] = {
    TestRTCAble.successful(testSpace.owner.mainIdentity)
  }
  
  def monitor(msg: => String):Unit = {}
  
  /**
   * Note that calling this only makes sense if you are operating in the same world as the specified App.
   * See querki.apps.SpaceInWorldWith to set this up properly.
   */
  def loadAppVersion(appId:OID, version:SpaceVersion, appsSoFar:Map[OID, SpaceState]):TCIdentity[SpaceState] = {
    val appSpace = testSpace.world.getSpace(appId)
    TestRTCAble.successful(appSpace.state)
  }
  
  def sendPublicationChanges(changes:List[SpaceEvent with UseKryo]):TCIdentity[CurrentPublicationState] = {
    // TODO: make this real
    TestRTCAble.successful(CurrentPublicationState(Map.empty))
  }
  
  def notifyPublished(who:User, thingId:OID)(implicit state:SpaceState):TCIdentity[PublishedAck] = {
    TestRTCAble.successful(PublishedAck())
  }
}

/**
 * This is the base concept of a TestSpace that is dynamically building a TestSpaceCore.
 * 
 * Also includes TestConversations, so it can be used for routing to those as well. It is gradually
 * becoming the faux version of SpaceRouter, routing messages to the right children as needed.
 */
abstract class SpaceCoreSpaceBase()(implicit val ecology:Ecology) extends TestSpace with TestConversations
{
  lazy val Types = interface[querki.types.Types]
  
  def sc:TestSpaceCore
  
  override def state = sc.currentState
  
  val oldSpaceOpt:Option[SpaceCoreSpaceBase] = None
  
  /**
   * This intentionally mimics SpaceRouter.
   */
  def !(msg:AnyRef):Option[AnyRef] = {
    msg match {
      case msg @ CurrentState(curState, _) => {
        routeToConv(msg)
      }
      case msg @ SpaceSubsystemRequest(_, _, payload:querki.conversations.messages.ConversationMessage) => routeToConv(msg)
      case msg:SpaceMessage => sc.aroundReceive(msg)
    }
  }
  
  def ?(msg:AnyRef):AnyRef = {
    (this ! msg).getOrElse(throw new Exception(s"Didn't get expected response to $msg"))
  }
  
  /**
   * Use this signature if you need to get a hold of the state after the change is made. Don't
   * over-use this -- there aren't many circumstances where you need it.
   */
  def addSomethingFull(name:String, kind:Kind, model:OID, propList:(OID, QValue)*):Option[AnyRef] = {
    val props = makePropFetcher(name, propList)
    this ! CreateThing(ownerRequest, sc.id, kind, model, props)
  }
  
  def addSomething(name:String, kind:Kind, model:OID, propList:(OID, QValue)*):OID = {
    val Some(ThingFound(oid, _)) = addSomethingFull(name, kind, model, propList:_*)
    oid
  }
  
  def addProperty[VT, RT](tpe:PType[VT] with PTypeBuilder[VT, RT], coll:Collection, name:String):PropRecord[VT, RT] = {
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
  
  def addType(name:String, modelId:OID) = {
    val Some(ThingFound(oid, state)) = addSomethingFull(name, Kind.Type, UrTypeOID, Types.ModelForTypeProp(modelId))
    state.typ(oid).asInstanceOf[ModelType]
  }
  
  def changeThing(thingId:OID, propList:(OID, QValue)*) = {
    this ! ChangeProps(owner, sc.id, thingId, Map(propList:_*))    
  }
}

/**
 * This is a specialized version of TestSpace that creates a SpaceCore and wraps around that. It allows a
 * much richer set of interactions than the traditional CommonSpace, but requires some setup effort.
 */
class SpaceCoreSpace(implicit e:Ecology) extends SpaceCoreSpaceBase {
  def configOpt:Option[TestSpaceConfig] = None
  lazy val world = new TestWorld
  val sc = new TestSpaceCore(spaceId, this, configOpt)
}

class SimpleCoreSpace(implicit e:Ecology) extends SpaceCoreSpace()(e) with querki.types.ModelTypeDefiner {
  // Boot the Space up
  this ! InitialState(owner, sc.id, "Test Space", owner.mainIdentity.id)
}

/**
 * This version of SpaceCoreSpaceBase starts from a previous one, and replays its history as the starting
 * point. It is intended to demonstrate that history replays properly.
 */
class ReplayCoreSpace(oldSpace:SpaceCoreSpaceBase)(implicit e:Ecology) extends SpaceCoreSpaceBase {
  
  override val oldSpaceOpt = Some(oldSpace)
  
  def world = oldSpace.world
  val sc = new TestSpaceCore(oldSpace.sc.id, this, oldSpace.sc.config, oldSpace.sc.history)
  override def makeOwner = oldSpace.owner
}

class SpaceCoreTests extends QuerkiTests {  
  "SpaceCoreSpace" should {
    // This used to throw an Exception, but the result was that, if a Space was corrupted, it
    // was completely stuck: you couldn't even open it to archive it. So we're now being more
    // forgiving, and just logging an error.
    "no longer throw an exception if it doesn't start with InitialState" in {
      implicit val s = new SpaceCoreSpace

      s ! CreateThing(s.ownerRequest, s.sc.id, Kind.Thing, SimpleThingOID, emptyProps)
    }
    
    // Test for the belt-and-suspenders check on QI.7w4g8ne:
    "throw an exception if it hits a duplicate OID" in {
      // This will begin duplicating OIDs after it hits nOIDs of them:
      class DuplicateOIDSpace(nOIDs:Int) extends SpaceCoreSpaceBase {
        import java.util.concurrent.atomic.AtomicInteger
        
        def configOpt:Option[TestSpaceConfig] = None
        lazy val world = new TestWorld
        
        lazy val nBase = 1000
        
        private def testOID(local:Int):OID = OID(2, local)
        private lazy val _currentLocal = new AtomicInteger(nBase + 1)
        
        override def toid():OID = {
          val next = _currentLocal.incrementAndGet()
          if (next > (nOIDs + nBase))
            _currentLocal.set(nBase + 1)
          testOID(next)
        }
        
        val sc = new TestSpaceCore(spaceId, this, configOpt)
      }
      // The 7 below assumes that building the Space + InitialState consumes 4 OIDs:
      implicit val s = new DuplicateOIDSpace(7)
      
      s ! InitialState(s.owner, s.sc.id, "Test Space", s.owner.mainIdentity.id)
      s ! CreateThing(s.ownerRequest, s.sc.id, Kind.Thing, SimpleThingOID, emptyProps)
      s ! CreateThing(s.ownerRequest, s.sc.id, Kind.Thing, SimpleThingOID, emptyProps)
      s ! CreateThing(s.ownerRequest, s.sc.id, Kind.Thing, SimpleThingOID, emptyProps)
      val caught = intercept[PublicException] {
        s ! CreateThing(s.ownerRequest, s.sc.id, Kind.Thing, SimpleThingOID, emptyProps)
      }
      assert(caught.msgName == "Space.createThing.OIDExists")
    }
  }
  
  "Reloading a Space" should {
    // Regression test for QI.bu6obzy
    "increment the snapshotCounter correctly when reloading" in {
      def doTest(maxHist:Int) = {
        class TestSpace extends SpaceCoreSpaceBase {
          // This is all about snapshotting, so pull the parameter in tight:
          def configOpt:Option[TestSpaceConfig] = Some(TestSpaceConfig(Some(maxHist)))
          lazy val world = new TestWorld
          val sc = new TestSpaceCore(spaceId, this, configOpt)
        }
        
        val s = new TestSpace
        s ! InitialState(s.owner, s.sc.id, "Test Space", s.owner.mainIdentity.id)
        s ! CreateThing(s.ownerRequest, s.sc.id, Kind.Thing, SimpleThingOID, emptyProps)
        s ! CreateThing(s.ownerRequest, s.sc.id, Kind.Thing, SimpleThingOID, emptyProps)
        
        val s2 = new ReplayCoreSpace(s)
        s2 ! CreateThing(s.ownerRequest, s.sc.id, Kind.Thing, SimpleThingOID, emptyProps)
        s2 ! CreateThing(s.ownerRequest, s.sc.id, Kind.Thing, SimpleThingOID, emptyProps)
        
        val s3 = new ReplayCoreSpace(s2)
        s3 ! CreateThing(s.ownerRequest, s.sc.id, Kind.Thing, SimpleThingOID, emptyProps)
        s3 ! CreateThing(s.ownerRequest, s.sc.id, Kind.Thing, SimpleThingOID, emptyProps)
        
        val s4 = new ReplayCoreSpace(s3)
        s4 ! CreateThing(s.ownerRequest, s.sc.id, Kind.Thing, SimpleThingOID, emptyProps)
        s4 ! CreateThing(s.ownerRequest, s.sc.id, Kind.Thing, SimpleThingOID, emptyProps)
        
        // Need to add 1 to maxHist, to account for the Snapshot:
        assert(s4.sc.history.length <= (maxHist + 1))
      }
      
      doTest(3)
      doTest(4)
      doTest(5)
    }
  }
}
