package querki.core

import models.{Kind, Thing}

import querki.ecology._
import querki.globals._
import querki.values.StateCacheKey

object FunctionMOIDs extends EcotIds(62) {
  val ImplementationModelOID = moid(1)
  val ImplementsFunctionPropOID = moid(2)
  val ImplementsTypePropOID = moid(3)
  val AbstractOverPropOID = moid(4)
  val AbstractOverModelOID = moid(5)
  val AbstractOverReceivedOID = moid(6)
}

/**
 * Used by the Abstract Function and Function Implementation system.
 */
case class FunctionImplsForOne(map: Map[OID, Thing]) extends AnyVal

case class FunctionImpls(map: Map[OID, FunctionImplsForOne]) extends AnyVal {

  def +(
    prop: Thing,
    implements: OID,
    types: List[OID]
  ): FunctionImpls = {
    val theseTypes = Map(types.map((_, prop)): _*)

    val newImpls = map.get(implements) match {
      case Some(FunctionImplsForOne(oneMap)) => oneMap ++ theseTypes
      case None                              => theseTypes
    }

    copy(map = map + (implements -> FunctionImplsForOne(newImpls)))
  }
}

/**
 * An Ecot that deals with function-centric stuff.
 *
 * @author jducoeur
 */
class FunctionEcot(e: Ecology) extends QuerkiEcot(e) with Functions {
  import FunctionMOIDs._

  /**
   * Enumeration that allows AbstractFunctions to say *which* element they abstract over. For
   * the moment we're only bothering to define AbstractOverReceived, but we might want to
   * add AbstractOverDefining and AbstractOverFirstParam down the road.
   */
  lazy val AbstractOverModel = new ThingState(
    AbstractOverModelOID,
    systemOID,
    RootOID,
    toProps(
      setName("_abstractOverModel"),
      setInternal
    )
  )

  /**
   * This AbstractFunction chooses its Type based on the Received value.
   */
  lazy val AbstractOverReceived = new ThingState(
    AbstractOverReceivedOID,
    systemOID,
    AbstractOverModelOID,
    toProps(
      setName("_abstractOverReceived"),
      setInternal
    )
  )

  override lazy val things = Seq(
    AbstractOverModel,
    AbstractOverReceived
  )

  /**
   * This is the base model for Implementation Functions. It should never be used as a Property itself.
   */
  lazy val ImplementationModel = new SystemProperty(
    ImplementationModelOID,
    YesNoType,
    ExactlyOne,
    toProps(
      setName("_implementationModel"),
      setInternal
    )
  )

  lazy val ImplementsFunctionProp = new SystemProperty(
    ImplementsFunctionPropOID,
    LinkType,
    ExactlyOne,
    toProps(
      setName("Implements Abstract Function"),
      AppliesToKindProp(Kind.Property),
      setInternal,
      Summary(
        "This should only be applied to Function Implementations, and says which Abstract Function this implements"
      )
    )
  )

  lazy val AbstractOverProp = new SystemProperty(
    AbstractOverPropOID,
    LinkType,
    ExactlyOne,
    toProps(
      setName("_abstractOver"),
      AppliesToKindProp(Kind.Property),
      setInternal,
      Summary("Says which field of an abstract function call should be used to derive its Type")
    )
  )

  lazy val ImplementsTypesProp = new SystemProperty(
    ImplementsTypePropOID,
    LinkType,
    QList,
    toProps(
      setName("_implementsTypes"),
      AppliesToKindProp(Kind.Property),
      setInternal,
      Summary("The OIDs of the PTypes or Models that this Implementation handles")
    )
  )

  override lazy val props = Seq(
    ImplementationModel,
    ImplementsFunctionProp,
    AbstractOverProp,
    ImplementsTypesProp
  )

  /**
   * *********************************************
   * FUNCTIONS
   * *********************************************
   */

  val funcImplCacheKey = StateCacheKey(MOIDs.ecotId, "FunctionImpls")

  def computeImplMap(state: SpaceState): FunctionImpls = {
    val impls =
      state.allProps.values.filter(_.model == ImplementationModelOID) ++
        state.allThings.filter(_.model == ImplementationModelOID)
    (FunctionImpls(Map.empty) /: impls) { (impls, prop) =>
      val propInfo = for {
        implementsPV <- prop.getPropOpt(ImplementsFunctionProp)(state)
        implements <- implementsPV.firstOpt
        typesPV <- prop.getPropOpt(ImplementsTypesProp)(state)
        types = typesPV.rawList
      } yield (implements, types)

      propInfo match {
        case Some((implements, types)) => impls + (prop, implements, types)
        case _                         => impls
      }
    }
  }

  def implMap(state: SpaceState): FunctionImpls = {
    state.fetchOrCreateCache(funcImplCacheKey, computeImplMap(state))
  }
}
