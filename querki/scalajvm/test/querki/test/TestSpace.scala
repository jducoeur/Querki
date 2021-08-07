package querki.test

import java.util.concurrent.atomic.AtomicInteger

import models._

import querki.core.MOIDs._
import querki.ecology._
import querki.spaces.CacheUpdate
import querki.types.{ModelTypeDefiner, ModeledPropertyBundle, SimplePropertyBundle}
import querki.values._
import querki.identity.{FullUser, Identity, IdentityKind, User, UserLevel}
import querki.identity.UserLevel._

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
  private def testOID(local: Int): OID = OID(2, local)

  private val _currentLocal = new AtomicInteger(0)

  /**
   * Construct the next OID available for this test run. OIDs are not guaranteed to be
   * stable from run to run, but will be unique within a single run.
   */
  def nextOID(): OID = testOID(_currentLocal.incrementAndGet())

  /**
   * Fetch a bunch of OIDs at once.
   */
  def oidBlock(nIds: Int): Seq[OID] = {
    for {
      n <- 0 to nIds
    } yield nextOID()
  }

  /**
   * The Spaces that exist in this World. Only really matters if you're playing with Apps.
   */
  private var spaceRegistry: Map[OID, TestSpace] = Map.empty

  /**
   * Registers this Space as belonging in this World. Happens automatically.
   */
  def register(space: TestSpace): Unit = {
    spaceRegistry += (space.spaceId -> space)
  }

  def getSpaceOpt(id: OID): Option[TestSpace] = spaceRegistry.get(id)

  def getSpace(id: OID): TestSpace = getSpaceOpt(id).getOrElse(
    throw new Exception(s"World didn't find a Space with id $id -- registry is $spaceRegistry")
  )
}

case class SpaceMember(
  user: User,
  person: ThingState
)

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
trait TestSpace extends EcologyMember with ModelTypeDefiner {

  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Core = interface[querki.core.Core]
  lazy val Person = interface[querki.identity.Person]
  lazy val SpaceChangeManager = interface[querki.spaces.SpaceChangeManager]
  lazy val System = interface[querki.system.System]

  lazy val ExactlyOne = Core.ExactlyOne
  lazy val Optional = Core.Optional
  lazy val QList = Core.QList
  lazy val QSet = Core.QSet

  // ================================
  //
  // COMMON TEST CLASSES
  //
  // These are several simplifications of the usual classes for defining Things. You
  // are not obligated to use them, but they will often be good enough, and much more
  // concise than using the full versions.
  //

  def makePropFetcher(
    name: String,
    pairs: Seq[(OID, QValue)]
  ) = {
    Core.toProps((pairs :+ Core.setName(name)): _*)
  }

  def registerType(pt: PType[_]) = { types = types :+ pt }

  def registerProp(p: Property[_, _]) = { props = props :+ p }

  class TestPropertyBase[VT, RT](
    pid: OID,
    t: PType[VT] with PTypeBuilder[VT, RT],
    c: Collection,
    p: PropMap
  ) extends Property[VT, RT](pid, spaceId, UrPropOID, t, c, p, querki.time.epoch) {
    lazy val iid: IndexedOID = IndexedOID(id)

    registerProp(this)
  }

  class TestProperty[VT, RT](
    t: PType[VT] with PTypeBuilder[VT, RT],
    c: Collection,
    name: String,
    pairs: (OID, QValue)*
  ) extends TestPropertyBase(toid(), t, c, makePropFetcher(name, pairs))

  def registerThing(t: ThingState) = {
    // Guard against accidental duplication:
    things.find(_.id == t.id) match {
      case Some(found) => throw new Exception(s"$t has the same OID as $found!")
      case None        => things = things :+ t
    }
  }

  class TestThingBase(
    pid: OID,
    name: String,
    model: OID,
    pairs: (OID, QValue)*
  ) extends ThingState(pid, spaceId, model, makePropFetcher(name, pairs)) {
    registerThing(this)
  }

  class UnnamedThing(
    model: OID,
    pairs: (OID, QValue)*
  ) extends ThingState(toid(), spaceId, model, Core.toProps(pairs: _*)) {
    registerThing(this)
  }

  class TestThing(
    pid: OID,
    name: String,
    model: OID,
    pairs: (OID, QValue)*
  ) extends TestThingBase(pid, name, model, pairs: _*) {
    def this(
      pid: OID,
      name: String,
      pairs: (OID, QValue)*
    ) = {
      this(pid, name, querki.basic.MOIDs.SimpleThingOID, pairs: _*)
    }
    def this(
      name: String,
      model: OID,
      pairs: (OID, QValue)*
    ) = {
      this(toid(), name, model, pairs: _*)
    }
  }

  class SimpleTestThing(
    name: String,
    pairs: (OID, QValue)*
  ) extends TestThing(toid, name, pairs: _*)

  class TestModel(
    name: String,
    pairs: (OID, QValue)*
  ) extends SimpleTestThing(name, (pairs :+ Core.IsModelProp(true)): _*)

  // Add this to the pairs for a Model or the otherSpaceProps in order to set up Instance Permissions
  // for it:
  def instancePermissions(pairs: (OID, QValue)*): (OID, QValue) = {
    val permThing = new UnnamedThing(AccessControl.InstancePermissionsModel, pairs: _*)
    AccessControl.InstancePermissionsProp(permThing)
  }

  object TestModelProperty {

    def apply(
      name: String,
      model: Thing,
      coll: Collection
    ): TestProperty[ModeledPropertyBundle, SimplePropertyBundle] = {
      val modelType = new ModelType(
        toid,
        model.id,
        Core.toProps(
          Core.setName(model.canonicalName + " Type")
        )
      )
      registerType(modelType)

      new TestProperty(modelType, coll, name)
    }
  }

  // ================================

  /**
   * The World for this Test. Concrete classes must instantiate this.
   */
  def world: TestWorld

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
  def toid(): OID = world.nextOID()

  world.register(this)

  /**
   * This Space's Apps, if any. Note that you specify the App's TestSpace; the state will be
   * automatically used.
   */
  def apps: Seq[TestSpace] = Seq.empty

  def userAs(
    name: String,
    handle: String,
    level: UserLevel
  ): User =
    FullUser(
      toid(),
      name,
      Seq(Identity(toid(), querki.email.EmailAddress(""), "", handle, name, IdentityKind.QuerkiLogin)),
      level
    )

  /**
   * Define a Member of this Space. Note that this *must* only be used in the Space's constructor, since
   * it registers the Member into the Space as a side-effect.
   *
   * For now, we only have one basic class of Member. Later, we'll have to get into groups/roles/etc.
   */
  def member(
    name: String,
    handle: String,
    level: UserLevel,
    pairs: (OID, QValue)*
  ): SpaceMember = {
    val user = userAs(name, handle, level)
    val person =
      new TestThing(name, AccessControl.PersonModel, (pairs :+ Person.IdentityLink(user.mainIdentity.id)): _*)
    SpaceMember(user, person)
  }

  def makeOwner: User = userAs("Owner Guy", "ownerHandle", PaidUser)
  lazy val owner: User = makeOwner

  lazy val spaceName = "Test Space"

  /**
   * The props of the Space itself. If you need these to be anything interesting, override
   * this and set it to what you want.
   */
  def otherSpaceProps: Seq[(OID, QValue)] = Seq.empty

  lazy val sProps: PropMap =
    Core.toProps(
      (otherSpaceProps :+ Core.setName(spaceName)): _*
    )

  /**
   * The PTypes introduced by this Test, if any.
   */
  var types: Seq[PType[_]] = Seq.empty

  /**
   * The Properties introduced by this Test, if any.
   */
  var props: Seq[Property[_, _]] = Seq.empty

  /**
   * The Things (usually Models, but not necessarily) introduced by this Test, if any.
   */
  var things: Seq[ThingState] = Seq.empty

  /**
   * The *INITIAL* state of this Space for the test. If you plan to evolve the State, the test
   * will need to fire up a Space Actor to manage that, or do it some other way.
   */
  lazy val _initState: SpaceState = {
    val s = SpaceState(
      spaceId, // This Space's OID
      System.State.id, // This Space's Model
      sProps, // This Space's own props
      owner.mainIdentity.id, // This Space's Owner
      spaceName, // This Space's Name
      querki.time.epoch, // This Space's last-modified time
      apps.map(_.state), // This Space's Apps, if any
      Some(System.State),
      OIDMap(types: _*),
      OIDMap(props: _*),
      OIDMap(things: _*),
      OIDMap(Seq.empty: _*), // This Space's Collections
      None // The Owner's actual Identity
    )

    val results = SpaceChangeManager.updateStateCache(CacheUpdate(None, None, s))

    results.current
  }

  /**
   * This is a def so that SpaceCoreSpace can be dynamic.
   */
  def state: SpaceState = _initState
}

class DynamicSpace(s: SpaceState)(implicit val ecology: Ecology) extends TestSpace {
  override lazy val state = s

  // In the simple case, we only have one Space, so it can own the World:
  lazy val world = new TestWorld
}
