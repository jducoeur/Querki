package models

import querki.globals._
import querki.values.{PropAndVal, QValue}

/**
 * This provides functions similar to PropertyBundle, for raw PropMaps. It is intentionally simpler, and
 * can't actually share the signature of PropertyBundle, because it doesn't require a SpaceState.
 *
 * TODO: this is all screaming out for refactoring, as part of making PropertyBundle into a typeclass. How
 * do we rectify the different requirements, though? Possibly the standard ThingBundle typeclass instance
 * would require an implicit SpaceState, where this one doesn't?
 */
class PropMapBundle(val props: PropMap) {

  def hasProp(propId: OID): Boolean = {
    props.contains(propId)
  }

  def getProp[VT, CT](prop: Property[VT, _]): PropAndVal[VT] = {
    localProp(prop).getOrElse(throw new Exception(s"getProp can't find Property ${prop.displayName}!"))
  }

  def getPropOpt[VT](prop: Property[VT, _]): Option[PropAndVal[VT]] = {
    if (hasProp(prop))
      Some(getProp(prop))
    else
      None
  }

  def localProp[VT, CT](prop: Property[VT, _]): Option[PropAndVal[VT]] = {
    prop.fromOpt(this.props).map(prop.pair)
  }

  def getFirstOpt[VT](prop: Property[VT, _]): Option[VT] = {
    getPropOpt(prop).flatMap(_.firstOpt)
  }

  def getFirst[VT](prop: Property[VT, _]): VT = {
    getFirstOpt(prop).getOrElse(
      throw new Exception(s"PropMapBundle.getFirst didn't find required Property ${prop.displayName}!")
    )
  }

  override def toString = {
    "PropMapBundle: " + props
  }
}
