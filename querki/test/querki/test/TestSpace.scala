package querki.test

import java.util.concurrent.atomic.AtomicInteger

import models.{OID, OIDMap}
import models.{Collection, Property, PType, PTypeBuilder, ThingState}
import models.Thing._

import models.system.{NameType, SystemSpace}
import models.system.OIDs.{PageOID, systemOID, UrPropOID}

import querki.values._

/**
 * This class represents the overall "world state" for a test. It usually works with at
 * least one TestSpace, but may be involved with any number of them.
 */
class TestWorld {
  
  /**
   * Construct an OID for a Thing in the Test Space. In general, you don't use this
   * method directly; instead, use toid() to generate OIDs.
   * 
   * IMPORTANT: Shard 2 is permanently reserved for Test Space, and should never be
   * used for an actual production Shard!
   */
  private def testOID(local:Int):OID = OID(2, local)
  
  private val _currentLocal = new AtomicInteger(0)
  
  /**
   * Construct the next OID available for this test run. OIDs are not guaranteed to be
   * stable from run to run, but will be unique within a single run.
   */
  def nextOID():OID = testOID(_currentLocal.incrementAndGet())
}

/**
 * This trait is used to build little Test Spaces for unit and functional testing.
 * In general, you assemble a TestSpace with the pieces you need to test, and pass
 * the resulting State into your test environment. The style is similar to that of
 * Modules, intentionally: like a Module, it is constructed a bunch of cohesive
 * elements for a purpose.
 * 
 * In general, you should construct a class based on TestSpace, that fills in the
 * bits you need.
 */
trait TestSpace {

  // ================================
  //
  // COMMON TEST CLASSES
  //
  // These are several simplifications of the usual classes for defining Things. You
  // are not obligated to use them, but they will often be good enough, and much more
  // concise than using the full versions.
  //
  
  def makePropFetcher(name:String, pairs:Seq[(OID,QValue)]) = {
    toProps((pairs :+ setName(name)):_*)
  }
  
  class TestPropertyBase[VT, -RT](pid:OID, t:PType[VT] with PTypeBuilder[VT, RT], c:Collection, p:PropFetcher) 
    extends Property[VT, RT](pid, spaceId, UrPropOID, t, c, p, modules.time.TimeModule.epoch)
  class TestProperty[VT, -RT](t:PType[VT] with PTypeBuilder[VT, RT], c:Collection, name:String, pairs:(OID,QValue)*)
    extends TestPropertyBase(toid(), t, c, makePropFetcher(name, pairs))
  
  class TestThing(pid:OID, name:String, pairs:(OID, QValue)*)
    extends ThingState(pid, spaceId, PageOID, makePropFetcher(name, pairs)) 
  {
    val testSpace:TestSpace = TestSpace.this 
    testSpace.things = testSpace.things :+ this
  }
  class SimpleTestThing(name:String, pairs:(OID, QValue)*)
    extends TestThing(toid, name, pairs:_*)
  
  // ================================
  
  /**
   * The World for this Test. Concrete classes must instantiate this.
   */
  val world:TestWorld
  
  /**
   * The OID of this Space.
   */
  lazy val spaceId = toid()
  
  /**
   * Get an OID for a test Thing.
   * 
   * This does not produce permanently-stable OIDs, and is not intended to. Instead,
   * it simply creates an OID that is unique for this specific test run.
   */
  def toid():OID = world.nextOID()
  
  /**
   * This Space's App. Defaults to the System Space; override this if you
   * want something different.
   */
  lazy val app:SpaceState = {
    SystemSpace.init
    SystemSpace.State
  }
  
  /**
   * The OID of this Space's Owner. Currently just a made-up value; override this if
   * you care. (This might get improved later, for testing security.)
   */
  lazy val ownerId:OID = toid()
  
  lazy val spaceName = "Test Space"
  
  /**
   * The props of the Space itself. If you need these to be anything interesting, override
   * this and set it to what you want.
   */
  val sProps:PropFetcher =
    toProps(
      setName(spaceName)
    )

  /**
   * The PTypes introduced by this Test, if any.
   */
  lazy val types:Seq[PType[_]] = Seq.empty
  /**
   * The Properties introduced by this Test, if any.
   */
  lazy val props:Seq[Property[_,_]] = Seq.empty
  /**
   * The Things (usually Models, but not necessarily) introduced by this Test, if any.
   */
  var things:Seq[ThingState] = Seq.empty
  
  /**
   * The *INITIAL* state of this Space for the test. If you plan to evolve the State, the test
   * will need to fire up a Space Actor to manage that.
   */
  lazy val state:SpaceState =
    SpaceState(
      toid(),    // This Space's OID
      app.id,    // This Space's Model
      sProps,    // This Space's own props
      ownerId,   // This Space's Owner
      spaceName, // This Space's Name
      modules.time.TimeModule.epoch, // This Space's last-modified time
      Some(app), // This Space's App
      OIDMap(types:_*),
      OIDMap(props:_*),
      OIDMap(things:_*),
      OIDMap(Seq.empty:_*),  // This Space's Collections
      None       // The Owner's actual Identity
    )
}
