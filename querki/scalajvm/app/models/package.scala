import querki.globals.Ecology
import querki.values.{QValue, SpaceState}

package object models {

  /**
   * Convenient alias for when you want to accept any Property, regardless of PType.
   */
  type AnyProp = Property[_, _]

  type PropMap = Map[OID, QValue]

  def emptyProps = Map.empty[OID, QValue]

  def toProps(pairs: (OID, QValue)*): PropMap = {
    (Map.empty[OID, QValue] /: pairs) { (m: Map[OID, QValue], pair: (OID, QValue)) =>
      m + (pair._1 -> pair._2)
    }
  }

  implicit def bundle2Ops(thing: PropertyBundle)(implicit ecology: Ecology): PropertyBundleOps = thing.thingOps(ecology)
  implicit def thing2Ops(thing: Thing)(implicit ecology: Ecology): ThingOps = thing.thingOps(ecology)
  implicit def space2Ops(state: SpaceState)(implicit ecology: Ecology) = state.spaceStateOps
  implicit def props2Bundle(props: PropMap) = new PropMapBundle(props)
}
