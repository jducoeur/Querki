package querki.data

import models.{Kind, Wikitext}

import querki.core.NameUtils

/**
 * Wrapper type, which represents a stringified ThingId. This should be
 * used in all APIs, and in the Client, so that we aren't passing anonymous Strings all
 * over the place. Note that this is an AnyVal, so should not incur overhead.
 */
case class TID(val underlying:String) extends AnyVal {
  def isEmpty:Boolean = underlying.length() == 0
}

/**
 * Variant of TID, to be used for times that can *only* contain an OID, not a
 * ThingName. This is simpler to process.
 */
case class TOID(val underlying:String) extends AnyVal {
  def isEmpty:Boolean = underlying.length() == 0  
}
object TOID {
  def apply(thing:BasicThingInfo):TOID = TOID(thing.oid.underlying)
}

trait BasicThingInfo {
  def oid:TID
  def linkName:Option[String]
  // This is the pre-neutered display name for this Thing. It should be rendered raw, without
  // further XML escaping:
  def displayName:String
  
  def urlName = linkName match {
    case Some(name) if (name.length() > 0) => TID(NameUtils.toUrl(name))
    case _ => oid
  }
  
  def is(other:BasicThingInfo) = oid == other.oid
}

/**
 * This is the API view of a Thing -- what it looks like on the wire, and in the Client.
 * Note that it is intentionally much less elaborate than the Server-side Thing.
 */
case class ThingInfo(
  oid:TID, 
  // TODO: this is duplicative, but it's so we can count on having an OID when we want it.
  // This wants to get rationalized with the oid field, which might be a name or an OID.
  // (It's really the ThingId.)
  oid2: TOID,
  linkName:Option[String],
  // We transmit the raw form, for the occasions where we need it.
  // Use this with great care, and always be sure to escape it when needed!
  // Use displayName instead in most cases.
  wikiName:Wikitext,
  modelOid:TID,
  kind:Kind.Kind,
  isModel:Boolean,
  // TODO: these should be rewritten as a permissions set, as in SpaceInfo:
  isEditable:Boolean,
  isDeleteable:Boolean,
  isInstantiatable:Boolean,
  isTag:Boolean,
  importedFrom:Option[SpaceInfo],
  // This is a collection of Boolean Property IDs; if present, that means it is set
  // to true on this Thing
  // TODO: several of the above should be rewritten to use this more-efficient approach!
  flags:Set[TOID],
  // This is a collection of Permissions that the requesting User has on this Thing.
  // TODO: same comments as for flags.
  perms:Set[TOID]) extends BasicThingInfo
{
  lazy val displayName = wikiName.strip.toString
  lazy val unsafeName = wikiName.plaintext
  def hasFlag(prop:ThingInfo) = flags.contains(TOID(prop))
  def hasPerm(perm:ThingInfo) = perms.contains(TOID(perm))
}

case class SpaceInfo(
  oid:TID, 
  linkName:Option[String], 
  displayName:String,
  ownerId:String,
  ownerHandle:String,
  apps:Seq[SpaceInfo],
  permissions:Set[TID],
  isApp:Boolean) extends BasicThingInfo

case class PropInfo(
  oid:TID,
  linkName:Option[String],
  displayName:String,
  appliesTo:Seq[Kind.Kind],
  collId:TID,
  // Note that the typeId points to the Type if it's conventional, or to the Model
  // iff it's a Model Property:
  typeId:TID,
  // True iff this Property is shadowing another:
  isShadow:Boolean
) extends BasicThingInfo

case class PropValInfo(
  propInfo:PropInfo,
  prompt:Option[Wikitext],
  renderedV:Wikitext,
  tooltip:Option[Wikitext],
  raw:String
)

case class SpaceProps(
  oid:TID,
  linkName:Option[String],
  displayName:String,
  standardProps:Seq[PropInfo],
  advancedProps:Seq[PropInfo],
  apps:Seq[SpaceProps]
) extends BasicThingInfo

case class CollectionInfo(
  oid:TID,
  linkName:Option[String],
  displayName:String
) extends BasicThingInfo

case class TypeInfo(
  oid:TID,
  linkName:Option[String],
  displayName:String,
  preferredCollection:Option[TID]
) extends BasicThingInfo

case class AllTypeInfo(
  collections:Seq[CollectionInfo],
  standardTypes:Seq[TypeInfo],
  advancedTypes:Seq[TypeInfo],
  models:Seq[ThingInfo]
)
