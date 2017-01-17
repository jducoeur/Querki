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
  importedFrom:Option[SpaceInfo]) extends BasicThingInfo
{
  lazy val displayName = wikiName.strip.toString
  lazy val unsafeName = wikiName.plaintext
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
