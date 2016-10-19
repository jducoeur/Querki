package querki.apps

import querki.globals._
import querki.spaces._
import querki.spaces.messages._
import querki.test._
import querki.values.SpaceVersion

/**
 * This is another Space in the same World as the other one. This is necessary for testing Apps.
 */
class SpaceInWorldWith(other:SpaceCoreSpaceBase)(implicit e:Ecology) extends SimpleCoreSpace {
  override lazy val world = other.world
  
  def addApp(app:SpaceCoreSpaceBase) = {
    this ! SpacePluginMsg(owner, sc.id, AddApp(app.sc.id, SpaceVersion(Int.MaxValue)))
  }
}

/**
 * Actually builds an App hierarchy for testing. Deliberately apes querki.spaces.AppTests.
 */
trait AppTree {
  def Basic:querki.basic.Basic
  
  implicit def ecology:Ecology
  
  lazy val highest = new SimpleCoreSpace {
    val highestThing = addSimpleThing("Highest Thing")
    val rootModel = addSimpleThing("My Root Model", Core.IsModelProp(true))
    val myNumProp = addProperty(Core.IntType, QList, "My Nums")
    val highestInstance = addThing("Highest Instance", rootModel)
  }
  
  lazy val mid1 = new SpaceInWorldWith(highest) {
    val duplicateThing = addSimpleThing("Duplicate Thing")
    val plusOne = addSimpleThing("Plus One", Basic.ApplyMethod("_plus(1)"))      
  }
  
  lazy val mid2 = new SpaceInWorldWith(highest) {
    val mySimplePage = addSimpleThing("Simple Page")
    val firstTwo = addSimpleThing("First Two", Basic.ApplyMethod("_take(2)"))
    
    addApp(highest)
    
    val mid2Instance = addThing("Mid2 Instance", highest.rootModel)      
  }
  
  lazy val mid3 = new SpaceInWorldWith(highest) {
    addApp(highest)
    
    val duplicateThing = addSimpleThing("Duplicate Thing")
    val numThing = addSimpleThing("Num Thing", highest.myNumProp(4, 6, 8))      
  }
  
  lazy val mainSpace = new SpaceInWorldWith(highest) {
    addApp(mid1)
//    addApp(mid2)
//    addApp(mid3)
//    
//    val mainInstance = addThing("Main Instance", highest.rootModel)
  }
}

class CoreAppTests extends QuerkiTests with AppTree {
  
  // Check that the specified name resolves to the specified Thing
  def testName(name:String, expected:OID)(implicit s:SpaceCoreSpaceBase) = {
    val tOpt = s.state.anythingByName(name)
    assert(tOpt.isDefined)
    assert(tOpt.get.id == expected)      
  }
  
  "A Space built with AddApp" should {
    "have the same characteristics as a hand-built one" in {
      implicit val s = mainSpace
      
//      testName("Simple Thing", Basic.SimpleThing)
    }
  }
}