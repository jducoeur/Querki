package querki.apps

import scala.util.{Success}

import org.scalatest.Assertions._

import models.Kind
import models.Thing.PropMap
import querki.globals._
import querki.identity.User
import querki.spaces._
import querki.spaces.messages._
import querki.test._
import querki.values.SpaceVersion

class AppableSpace(implicit e:Ecology) extends SimpleCoreSpace with AppExtractorSupport[TCIdentity] {
  lazy val Apps = interface[Apps]
  lazy val IdentityAccess = interface[querki.identity.IdentityAccess]
  
  def makeThisAnApp() = {
    val permProps = Apps.CanUseAsAppPerm(AccessControl.PublicTag)
    changeThing(state.id, permProps)
  }
  
  def success[T](v:T):TCIdentity[T] = new TCIdentity(Success(v))
  
  /**
   * This is the object you use to play with extracting an App from this Space.
   */
  def makeExtractor():AppExtractor[TCIdentity] = {
    new AppExtractor(state, owner)(TestRTCAble, this)
  }
  
  /* **************************************
   * AppExtractorSupport methods. You don't call these directly; they are used inside AppExtractor.
   */
  
  def getOIDs(nRequested:Int):TCIdentity[Seq[OID]] = {
    success(world.oidBlock(nRequested))
  }
  def createSpace(user:User, spaceId:OID, name:String, display:String):TCIdentity[OID] = {
    val app = new SpaceInWorldWith(this, Some(spaceId))
    success(app.spaceId)
  }
  def setAppState(state:SpaceState):TCIdentity[SpaceState] = {
    world.getSpace(state.id) match {
      case app:SpaceCoreSpaceBase => {
        (app ! SetState(owner, state.id, state)).get match {
          case ThingFound(_, newState) => success(newState)
          case other => throw new Exception(s"setAppState got unexpected return value $other")
        }
      }
      case _ => throw new Exception(s"Trying to set appState for non-Core Space ${state.id}!")
    }
  }
  def setChildState(state:SpaceState):TCIdentity[Any] = {
    success((this ! SetState(owner, state.id, state)).get)
  }
  def sendSpaceMessage(msg:SpaceMessage):TCIdentity[OID] = {
    val targetSpaceId = msg.spaceId
    val targetSpace = world.getSpaceOpt(targetSpaceId) match {
      case Some(g:SpaceCoreSpaceBase) => g
      case _ => new SpaceInWorldWith(this, Some(targetSpaceId))
    }
    val result = targetSpace ! msg
    val resultId = result match {
      case Some(ThingAck(id)) => id
      case other => throw new Exception(s"sendSpaceMessage($msg) returned $other")
    }
    success(resultId)    
  }
  
  /* ************************************** */
}

/**
 * This is another Space in the same World as the other one. This is necessary for testing Apps.
 * 
 * Note that you *can* preset the ID of this Space; that's needed for the ExtractApp workflow.
 */
class SpaceInWorldWith(other:SpaceCoreSpaceBase, presetSpaceId:Option[OID] = None)(implicit e:Ecology) extends AppableSpace {
  override lazy val world = other.world
  override lazy val spaceId = presetSpaceId.getOrElse(toid())
  
  def addApp(app:SpaceCoreSpaceBase) = {
    (this ! SpacePluginMsg(owner, sc.id, AddApp(app.sc.id, SpaceVersion(Int.MaxValue)))) match {
      case Some(AddAppResult(exOpt)) => exOpt.map { ex => throw ex }
      case other => throw new Exception(s"addApp() received unexpected result $other")
    }
  }
}

class SpaceInWorldWithSnapshots(other:SpaceCoreSpaceBase, interval:Int)(implicit e:Ecology) extends SpaceInWorldWith(other) {
  override def configOpt = Some(TestSpaceConfig(Some(interval)))
}

/**
 * Actually builds an App hierarchy for testing. Deliberately apes querki.spaces.AppTests.
 */
trait AppTree {
  def Basic:querki.basic.Basic
  
  implicit def ecology:Ecology
  
  lazy val highest = new AppableSpace {
    val highestThing = addSimpleThing("Highest Thing")
    val rootModel = addSimpleThing("My Root Model", Core.IsModelProp(true))
    val myNumProp = addProperty(Core.IntType, QList, "My Nums")
    val highestInstance = addThing("Highest Instance", rootModel)
    
    makeThisAnApp()
  }
  
  lazy val mid1 = new SpaceInWorldWith(highest) {
    val duplicateThing = addSimpleThing("Duplicate Thing")
    val plusOne = addSimpleThing("Plus One", Basic.ApplyMethod("_plus(1)"))
    
    makeThisAnApp()
  }
  
  lazy val mid2 = new SpaceInWorldWith(highest) {
    val mySimplePage = addSimpleThing("Simple Page")
    val firstTwo = addSimpleThing("First Two", Basic.ApplyMethod("_take(2)"))
    
    addApp(highest)
    
    val mid2Instance = addThing("Mid2 Instance", highest.rootModel)
    
    makeThisAnApp()
  }
  
  lazy val mid3 = new SpaceInWorldWith(highest) {
    addApp(highest)
    
    val duplicateThing = addSimpleThing("Duplicate Thing")
    val numThing = addSimpleThing("Num Thing", highest.myNumProp(4, 6, 8))
    
    makeThisAnApp()
  }
  
  lazy val mainSpace = new SpaceInWorldWith(highest) {
    addApp(mid1)
    addApp(mid2)
    addApp(mid3)
    
    val mainInstance = addThing("Main Instance", highest.rootModel)
  }
}

class CoreAppTests extends QuerkiTests with AppTree {
  
  // Check that the specified name resolves to the specified Thing
  def testName(name:String, expected:OID)(implicit s:SpaceCoreSpaceBase) = {
    val tOpt = s.state.anythingByName(name)
    assert(tOpt.isDefined)
    assert(tOpt.get.id == expected, s"Failed when trying to test name $name")
  }
  
  "A Space built with AddApp" should {
    // This basically runs through most of the stuff in the old querki.spaces.AppTests
    "have the same characteristics as a hand-built one" in {
      implicit val s = mainSpace
      implicit val state = s.state
      
      testName("Simple Thing", Basic.SimpleThing)
      // Pages -- simple Things -- get shadowed in the child Space:
      pql("""[[Highest Thing -> _shadowedThing -> _is(Highest Thing)]]""") should equal("false")
      pql("""[[Simple Page -> _shadowedThing -> _is(Simple Page)]]""") should equal("false")
      pql("""[[Duplicate Thing -> _shadowedThing -> _is(Duplicate Thing)]]""") should equal("false")
      pql("[[Num Thing -> My Nums -> Plus One -> First Two -> _commas]]") should
        equal("5, 7")
      pql("[[My Root Model._instances]]") should
        equal(listOfLinkText(highest.highestInstance, mainSpace.mainInstance, mid2.mid2Instance))
    }
  }
}
