package querki.spaces

import models._
import Kind.Kind

import querki.apps.AppsPure
import querki.core.NameUtils
import querki.globals._
import querki.identity.Identity
import querki.identity.IdentityPersistence.UserRef
import querki.spaces.SpaceMessagePersistence._
import querki.time.DateTime
import querki.types.ModelTypeBase
import querki.types.MOIDs.ModelForTypePropOID
import querki.values.{SpaceState, SpaceVersion}

/**
 * This trait represents the heart of the processing of Space messages: the
 * pure functions that take a Space and return a new Space. They are separated
 * out so that we can handle this process in various ways.
 *
 * Note that this class is intentionally stateless: it contains all the functions
 * about how to *transform* a SpaceState based on its events.
 */
trait SpacePure extends AppsPure with querki.types.ModelTypeDefiner with ModelPersistence with EcologyMember {

  private lazy val Basic = interface[querki.basic.Basic]
  private lazy val Core = interface[querki.core.Core]
  private lazy val System = interface[querki.system.System]

  private lazy val SystemState = System.State
  def id: OID

  lazy val emptySpace =
    SpaceState(
      id,
      SystemState.id,
      emptyProps,
      UnknownOID,
      "",
      DateTime.now,
      Seq.empty,
      Some(SystemState),
      Map.empty,
      Map.empty,
      Map.empty,
      Map.empty,
      None,
      SpaceVersion(0)
    )

  def initStatePure(
    userId: OID,
    ownerId: OID,
    identityOpt: Option[Identity],
    display: String
  ): SpaceState = {
    val canonical = NameUtils.canonicalize(display)
    val initState =
      SpaceState(
        id,
        SystemState.id,
        Map(
          Core.NameProp(canonical),
          Basic.DisplayNameProp(display)
        ),
        ownerId,
        canonical,
        DateTime.now,
        Seq.empty,
        Some(SystemState),
        Map.empty,
        Map.empty,
        Map.empty,
        Map.empty,
        identityOpt,
        SpaceVersion(0)
      )
    initState
  }

  def basedOn(props: PropMap): Option[OID] = {
    val result = for {
      basedOnVal <- props.get(ModelForTypePropOID)
      basedOn <- basedOnVal.firstAs(Core.LinkType)
    } yield basedOn

    result
  }

  /**
   * This is the pure-functional heart of creation: it takes a SpaceState and the key info, and returns the
   * modified SpaceState.
   */
  def createPure(
    creator: UserRef,
    kind: Kind,
    thingId: OID,
    modelId: OID,
    props: PropMap,
    modTime: DateTime
  )(
    state: SpaceState
  ): SpaceState = {
    kind match {
      case Kind.Thing => {
        val thing = ThingState(thingId, state.id, modelId, props, modTime, kind, Some(creator), Some(modTime))
        state.copy(things = state.things + (thingId -> thing))
      }
      case Kind.Property => {
        val typ = state.typ(Core.TypeProp.first(props))
        val coll = state.coll(Core.CollectionProp.first(props))
        val boundTyp = typ.asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]]
        val boundColl = coll.asInstanceOf[Collection]
        val thing = Property(thingId, state.id, modelId, boundTyp, boundColl, props, modTime)
        state.copy(spaceProps = state.spaceProps + (thingId -> thing))
      }
      case Kind.Type => {
        // Note that basedOn() returns an Option, but we should have sanity-checked this in
        // createSomething, so we *expect* it to always be defined. Let's not risk a crash
        // unnecessarily, though:
        basedOn(props).map { typBasedOn =>
          val tpe = new ModelType(thingId, state.id, querki.core.MOIDs.UrTypeOID, typBasedOn, props)
          state.copy(types = state.types + (thingId -> tpe))
        }.getOrElse(state)
      }
      case _ => {
        QLog.error(s"SpacePure.createPure is trying to create something of kind $kind!")
        // This shouldn't be possible -- we're checking against it in createSomething()
        state
      }
    }
  }

  /**
   * Given an existing PropMap, and a set of changes, produce the resulting PropMap.
   */
  def enhanceProps(
    oldProps: PropMap,
    newProps: PropMap
  ): PropMap = {
    (oldProps /: newProps) { (current, pair) =>
      val (propId, v) = pair
      if (v.isDeleted)
        // The caller has sent the special signal to delete this Property:
        current - propId
      else
        current + pair
    }
  }

  /**
   * This is the pure-functional core of the modify operation, which takes a SpaceState and returns a modified one.
   *
   * TODO: once upon a time, there was a horrible mechanism for dealing with Properties changing their Types, based on
   * the PropTypeMigrator Ecot. That Ecot has (as of this writing) been deprecated, and should be removed in due
   * course, because it was a horrible way to deal with the problem.
   */
  def modifyPure(
    thingId: OID,
    thing: Thing,
    modelIdOpt: Option[OID],
    newProps: PropMap,
    replaceAllProps: Boolean,
    modTime: DateTime
  )(
    state: SpaceState
  ): SpaceState = {
    val actualProps =
      if (replaceAllProps)
        newProps
      else {
        enhanceProps(thing.props, newProps)
      }

    val modelId = modelIdOpt match {
      case Some(m) => m
      case None    => thing.model
    }

    thing match {
      case t: ThingState => {
        val newThingState = t.copy(m = modelId, pf = actualProps, mt = modTime)
        state.copy(things = state.things + (thingId -> newThingState))
      }
      case prop: Property[_, _] => {
        val newProp = prop.copy(m = modelId, pf = actualProps, mt = modTime)
        state.copy(spaceProps = state.spaceProps + (thingId -> newProp))
      }
      case s: SpaceState => {
        // Note that the actual notification to the MySQL layer happens in SpaceCore.modifyWithSpaceName
        // TODO: handle changing the owner or apps of the Space. (Different messages?)
        val newNameOpt = for {
          rawName <- Core.NameProp.firstOpt(actualProps)
          newName = NameUtils.canonicalize(rawName)
          oldName = Core.NameProp.first(thing.props)
          if (!NameUtils.equalNames(newName, oldName))
        } yield newName
        val newName = newNameOpt.getOrElse(state.name)
        state.copy(m = modelId, pf = actualProps, name = newName, mt = modTime)
      }
      case mt: ModelTypeBase => {
        // Note that ModelTypeBase has a copy() method tuned for this purpose:
        val newType = mt.copy(modelId, actualProps)
        state.copy(types = state.types + (thingId -> newType))
      }
    }
  }

  def deletePure(
    oid: OID,
    thing: Thing
  )(
    state: SpaceState
  ): SpaceState = {
    thing match {
      case t: ThingState     => state.copy(things = state.things - oid)
      case p: Property[_, _] => state.copy(spaceProps = state.spaceProps - oid)
      // This really shouldn't be possible, since deleteThing is looking it up from just the
      // things and spaceProps tables:
      // TODO (QI.7w4g7x5): deal with deleting a Model Type:
      case _ => throw new Exception("Somehow got a request to delete unexpected thing " + thing)
    }
  }

  /**
   * This is the core of processing a Space's history -- a PartialFunction over Space events,
   * which takes the current history as a curried parameter.
   */
  def evolveStateCore(stateOpt: Option[SpaceState]): PartialFunction[SpaceEvent, SpaceState] = {
    case DHSetState(dh, modTime, reason, details) => rehydrate(dh)

    case DHInitState(userRef, display) => initStatePure(userRef.userId, userRef.identityIdOpt.get, None, display)

    case DHCreateThing(req, thingId, kind, modelId, dhProps, modTime, restored) => {
      implicit val s = stateOpt.get
      createPure(req, kind, thingId, modelId, dhProps, modTime)(s)
    }

    case DHModifyThing(req, thingId, modelIdOpt, propChanges, replaceAllProps, modTime) => {
      implicit val s = stateOpt.get
      s.anything(thingId).map { thing =>
        modifyPure(thingId, thing, modelIdOpt, propChanges, replaceAllProps, modTime)(s)
      }.getOrElse(s)
    }

    case DHDeleteThing(req, thingId, modTime) => {
      implicit val s = stateOpt.get
      s.anything(thingId).map { thing =>
        deletePure(thingId, thing)(s)
      }.getOrElse(s)
    }
  }

  /**
   * Note that we encapsulate in some specialized functionality from subsystems!
   */
  def evolveState(stateOpt: Option[SpaceState]): PartialFunction[SpaceEvent, SpaceState] =
    evolveStateCore(stateOpt).orElse(evolveStateApps(stateOpt))
}
