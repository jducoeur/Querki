package querki.spaces

import querki.test._

import models.{OID, Thing}
import models.Thing.PropMap

import querki.ecology._
import querki.time.DateTime
import querki.types.{ComplexSpace, ModelTypeDefiner}
import querki.values.{QValue, SpaceState}

/**
 * A stub for the "database" in Space saving/loading. Plays at least somewhat fair: truly serializes and
 * deserializes the Things.
 */
class SpaceLoadTestDB(val ecology:Ecology) extends EcologyMember with ThingStreamLoader {
  case class StoredThing(id:Long, kind:Int, model:Long, modified:Long, props:String)
  
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  
  var db:Map[Long, StoredThing] = Map.empty
  
  def store(t:Thing)(implicit state:SpaceState) = {
    db = db + (t.id.raw -> StoredThing(t.id.raw, t.kind, t.model.raw, t.modTime.getMillis, SpacePersistence.serializeProps(t.props, state)))
  }
  
  def storeSpace(implicit state:SpaceState) = {
    store(state)
    state.types.values.foreach(store(_))
    state.spaceProps.values.foreach(store(_))
    state.things.values.foreach(store(_))
  }
  
  def getThingStream[T <: Thing](kind:Int)(state:SpaceState)(builder:(OID, OID, PropMap, DateTime) => T):Stream[T] = {
    val stores = db.values.filter(_.kind == kind).toStream
    stores.map(store => builder(OID(store.id), OID(store.model), SpacePersistence.deserializeProps(store.props, state), new DateTime(store.modified)))
  }
}

/**
 * Stub for testing the SpaceLoader.
 */
class SpaceLoadTester(val state:SpaceState, val ecology:Ecology) extends SpaceLoader with EcologyMember with ModelTypeDefiner {
  
  // ======================
  // Values expected by SpaceLoader
  //
  lazy val Core = interface[querki.core.Core]
  lazy val SystemInterface = interface[querki.system.System]
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  lazy val Types = interface[querki.types.Types]
  
  def id:OID = state.id
  def name:String = state.name
  def owner:OID = state.owner

  // =======================
  
}

class SpaceLoadTests extends QuerkiTests {
  def saveLoad(space:CommonSpace):SpaceState = {
    val userTesting = interface[UserTesting]
    val db = new SpaceLoadTestDB(ecology)
    val loader = new SpaceLoadTester(commonState, ecology)
      
    userTesting.prepSpace(space)
    db.storeSpace(space.state)
    loader.doLoad(db)
  }
  
  "SpacePersister" should {
    "successfully save and load the CommonSpace" in {
      val loadedState = saveLoad(commonSpace)
      
      // Just do a simple smoketest based on the contents of CommonSpace. This demonstrates that both
      // the property and instance have loaded:
      processQText(loadedContext(loadedState, commonSpace.instance.id), """[[My Optional Text]]""") should
        equal ("""Hello world""")
    }
    
    "successfully save and load if the Space uses its own Property" in {
      class TestSpace extends CommonSpace {
        override def otherSpaceProps:Seq[(OID, QValue)] = Seq(optTextProp("I'm a Space!"))
      }
      val space = new TestSpace
      
      val loadedState = saveLoad(space)
      
      // Just do a simple smoketest based on the contents of CommonSpace. This demonstrates that both
      // the property and instance have loaded:
      processQText(loadedContext(loadedState, space.state.id), """[[My Optional Text]]""") should
        equal ("""I'm a Space!""")
    }
    
    "successfully save and load a Space with Model Types" in {
      val space = new ComplexSpace
      
      val loadedState = saveLoad(space)
      
      processQText(loadedContext(loadedState, space.thingWithComplex.id), """[[Complex Prop -> Text in Model]]""") should
        equal ("Text in Instance")      
      
      processQText(loadedContext(loadedState, space.thingWithComplex.id), """[[Top Level Thing -> Meta Property -> _first -> Complex Prop -> Text in Model]]""") should
        equal ("Top Text 1")      
      
      processQText(loadedContext(loadedState, space.thingWithComplex.id), """[[My Tree -> Left -> Right -> Node Id]]""") should
        equal ("3")      
    }
  }
}
