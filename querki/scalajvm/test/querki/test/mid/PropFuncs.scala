package querki.test.mid

import org.scalatest.Matchers._
import cats.effect.IO
import autowire._
import querki.api.ThingFunctions
import querki.data._
import querki.editing.EditFunctions
import querki.editing.EditFunctions._
import querki.globals._
import AllFuncs._
import org.scalactic.source.Position

trait PropFuncs {

  /**
   * The Collections, for use when creating a new Property.
   */
  val exactlyOne: TestOp[ThingInfo] = fetchStd(_.core.exactlyOneColl)
  val optional: TestOp[ThingInfo] = fetchStd(_.core.optionalColl)
  val qlist: TestOp[ThingInfo] = fetchStd(_.core.listColl)
  val qset: TestOp[ThingInfo] = fetchStd(_.core.setColl)

  /**
   * Common Types, for use when creating a new Property.
   */
  val tagType: TestOp[TID] = getType("Tag Type")
  val textType: TestOp[TID] = getType("Text Type")

  /**
   * Create the specified Property. This version takes pre-fetched Collection and PType.
   */
  def makeProperty(
    name: String,
    coll: ThingInfo,
    tpe: TID
  ): TestOp[TID] = {
    for {
      collProp <- fetchStd(_.core.collectionProp)
      tpeProp <- fetchStd(_.core.typeProp)
      nameProp <- fetchStd(_.core.nameProp)
      propModel <- fetchStd(_.core.urProp)
      propId <- createThing(propModel, nameProp :=> name, tpeProp :=> tpe, collProp :=> coll)
    } yield propId
  }

  /**
   * Create the specified Property. This version takes TestOp parameters, to make it convenient
   * to use the built-ins defined in PropFuncs.
   */
  def makeProperty(
    name: String,
    collOp: TestOp[ThingInfo],
    tpeOp: TestOp[TID]
  ): TestOp[TID] = {
    for {
      coll <- collOp
      tpe <- tpeOp
      propId <- makeProperty(name, coll, tpe)
    } yield propId
  }

  /**
   * Adds the specified Property to the specified Thing. The resulting structure can be used to set the
   * Property's value.
   */
  def addProperty(
    thingId: TID,
    propId: TID
  ): TestOp[PropEditInfo] = {
    for {
      propEditInfo <- TestOp.client { _[EditFunctions].addPropertyAndGetEditor(thingId, propId).call() }
    } yield propEditInfo
  }

  /**
   * Encapsulates the common workflow of adding a Property to the Thing, and editing it to set its value.
   * This pretty faithfully represents the way we usually add Properties to Models.
   */
  def addAndSetProperty[T : Saveable](
    thingId: TID,
    propId: TID,
    v: T
  ): TestOp[Unit] = {
    for {
      info <- addProperty(thingId, propId)
      _ <- changeProp(thingId, info, v)
    } yield ()
  }

  /**
   * Basically asserts that the specified String-valued Property has the expected value.
   *
   * This needs a bit of generalization, to work with other value types.
   */
  def checkPropValue(
    thingId: TID,
    propId: TID,
    expected: String
  )(implicit
    position: Position
  ): TestOp[Unit] = {
    val propOid = TOID(propId.underlying)
    for {
      valueMap <- TestOp.client { _[ThingFunctions].getPropertyValues(thingId, List(propOid)).call() }
      v = valueMap.get(propOid).getOrElse(fail(s"Failed to find a value for $thingId.$propId!"))
      _ = v.vs.head should be(expected)
    } yield ()
  }

  ////////////////////////////////////
  //
  // Internals
  //

  /**
   * You don't call this directly; access types through getType(), which will populate the cache
   * if needed.
   */
  private val fetchAllTypes: TestOp[AllTypeInfo] = {
    for {
      info <- TestOp.client { _[ThingFunctions].getAllTypes().call() }
      // Cache the type info in the WorldState:
      _ <- TestOp.update(TestState.systemL.modify(_.copy(typeInfoOpt = Some(info))))
    } yield info
  }

  private def typeFromCache(
    name: String,
    allInfo: AllTypeInfo
  ): TID = {
    allInfo.standardTypes.find(_.displayName == name)
      .orElse(allInfo.advancedTypes.find(_.displayName == name))
      .getOrElse(throw new Exception(s"Type $name not found!"))
      .oid
  }

  /**
   * For the most part, test code shouldn't bother calling this directly. Instead, use
   * the Common Types list above, and add to it if needed. (For the moment, this is
   * defined as private, until I have a reason to believe otherwise.)
   */
  private def getType(name: String): TestOp[TID] = {
    for {
      typeInfoOpt <- TestOp.fetch(_.world.system.typeInfoOpt)
      tid <-
        typeInfoOpt match {
          case Some(allInfo) => TestOp.pure(typeFromCache(name, allInfo))
          case None => {
            fetchAllTypes.map(allInfo => typeFromCache(name, allInfo))
          }
        }
    } yield tid
  }
}

object PropFuncs extends PropFuncs
