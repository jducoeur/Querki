package models

import querki.values.{QValue, SpaceState}

/**
 * IMPORTANT: there are now two indexes here. "index" is the index of this value within the *containing* Property, if any -- that
 * is, if this is in a List of Model Type, it is the index into that *parent* List. "listIndex" is the index of this value within
 * *this* Property, when we are talking about an ElemValue.
 *
 * TODO: this is arguably broken as sin, and mostly reflects the history that we were sending entire Lists around, rather than elements.
 * It should probably get heavily re-written.
 */
class FieldIds(
  val bundleOpt: Option[PropertyBundle],
  val p: Property[_, _],
  val container: Option[FieldIds] = None,
  val index: Option[Int] = None,
  val listIndex: Option[Int] = None
) {
  lazy val propId = p.id.toString
  lazy val propIdWithIndex = propId + index.map("[" + _.toString + "]").getOrElse("")

  def idStack(
    parent: Option[FieldIds],
    soFar: String,
    withThing: Boolean
  ): String = {
    val wrappedSoFar = {
      if (soFar.length() > 0)
        propIdWithIndex + "-" + soFar
      else
        propIdWithIndex
    }
    parent match {
      case Some(p) => p.idStack(p.container, wrappedSoFar, withThing)
      case None    => wrappedSoFar + { if (withThing) "-" + plainThingId else "" }
    }
  }

  lazy val fullPropId = idStack(container, "", false)

  /**
   * Note that this produces an undotted version of the name, whereas thingId produces the
   * more normal format.
   */
  private lazy val plainThingId: String = {
    val resultOpt = for {
      bundle <- bundleOpt
      thing <- bundle.asThing
    } yield thing.id.toString

    resultOpt.orElse(container.map(_.plainThingId)).getOrElse("")
  }

  lazy val thingId: Option[String] = {
    val resultOpt: Option[String] = for {
      bundle <- bundleOpt
      thing <- bundle.asThing
    } yield thing.id.toThingId

    resultOpt.orElse(container.flatMap { cont => cont.thingId })
  }

  lazy val suffix = "-" + idStack(container, "", true)

  lazy val inputControlId = "v" + suffix
  lazy val collectionControlId = "coll" + suffix
  // This is a hidden input field, which is a flag to tell the receiving code whether the
  // field is "empty" -- inherited or deleted, but with no local value:
  lazy val emptyControlId = "empty" + suffix

  override def toString = idStack(container, "", true)

  // HACK: to avoid breaking other code, keep listIndex relatively isolated for now:
  def withListIndex = idStack(container, "", true) + (listIndex match {
    case Some(i) => s"-item[$i]"
    case None    => ""
  })
}

object FieldIds {

  def apply(
    t: Option[Thing],
    p: Property[_, _]
  ) = new FieldIds(t, p)
}

case class DisplayPropVal(
  on: Option[PropertyBundle],
  prop: Property[_, _],
  v: Option[QValue],
  inheritedVal: Option[QValue] = None,
  inheritedFrom: Option[Thing] = None,
  cont: Option[DisplayPropVal] = None,
  i: Option[Int] = None
) extends FieldIds(on, prop, cont, i) {
  lazy val isInherited = v.isEmpty && inheritedVal.isDefined

  lazy val hasInheritance = inheritedVal.isDefined

  lazy val effectiveV = v.orElse(inheritedVal)
}

object DisplayPropVal {

  private def propPathFromSuffix(
    suffixIn: String,
    bundle: Option[PropertyBundle]
  )(implicit
    state: SpaceState
  ): Option[FieldIds] = {
    def getSuffix(suffixStr: String): (String, Option[Int]) = {
      val suffixLen = suffixStr.length
      (
        suffixIn.substring(0, suffixIn.indexOf(suffixStr)),
        Some(java.lang.Integer.parseInt(suffixIn.substring(suffixIn.indexOf(suffixStr) + suffixLen).dropRight(1)))
      )
    }

    // HACK: to work around the funny values produced by Manifest, when it is dynamically serializing.
    // Fix this on the client side!
    val (suffix, listIndex) =
      if (suffixIn.contains("_values["))
        getSuffix("_values[")
      else if (suffixIn.contains("-item["))
        // It is deeply sad that we need to know this.
        // TODO: merge QList's notion of indexes with the one in here, to make things consistent!
        getSuffix("-item[")
      else
        (suffixIn, None)

    val path = suffix.split("-").map(IndexedOID.parse(_))
    if (path.exists(_.isEmpty))
      // Something didn't parse, so it's not a legal path:
      None
    else {
      // Drop the Thing ID at the right-hand end:
      val propIds =
        if (suffix.endsWith("-"))
          // There is no ThingID -- this is common when you are creating a new Thing:
          path.flatten
        else
          path.flatten.dropRight(1)
      (Option.empty[FieldIds] /: propIds) { (current, propId) =>
        val prop = state.prop(propId.id).get
        Some(new FieldIds(bundle, prop, current, propId.i, listIndex))
      }
    }
  }

  /**
   * Given a field name from the public document (which might originate from inputControlId or emptyControlId),
   * try to parse it, and pull out the "property" part of the name.
   *
   * This is used in the Application code, to figure out which properties were being edited in the browser.
   */
  def propPathFromName(
    publicName: String,
    bundle: Option[PropertyBundle]
  )(implicit
    state: SpaceState
  ): Option[FieldIds] = {
    if (publicName.startsWith("v-"))
      propPathFromSuffix(publicName.substring(2), bundle)
    else if (publicName.startsWith("empty-"))
      propPathFromSuffix(publicName.substring(6), bundle)
    else if (publicName.startsWith("coll-"))
      propPathFromSuffix(publicName.substring(5), bundle)
    else
      // This clearly isn't the right format, on its face:
      None
  }
}
