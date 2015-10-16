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
  def displayName:String
  
  def urlName = linkName match {
    case Some(name) => TID(NameUtils.toUrl(name))
    case None => oid
  }
}

/**
 * This is the API view of a Thing -- what it looks like on the wire, and in the Client.
 * Note that it is intentionally much less elaborate than the Server-side Thing.
 */
case class ThingInfo(
  oid:TID, 
  linkName:Option[String], 
  displayName:String,
  modelOid:TID,
  kind:Kind.Kind,
  isModel:Boolean,
  isEditable:Boolean,
  isDeleteable:Boolean,
  isInstantiatable:Boolean,
  isTag:Boolean,
  importedFrom:Option[SpaceInfo]) extends BasicThingInfo

case class SpaceInfo(
  oid:TID, 
  linkName:Option[String], 
  displayName:String,
  ownerId:String,
  ownerHandle:String) extends BasicThingInfo

case class PropInfo(
  oid:TID,
  linkName:Option[String],
  displayName:String,
  appliesTo:Option[Kind.Kind],
  collId:TID,
  // Note that the typeId points to the Type if it's conventional, or to the Model
  // iff it's a Model Property:
  typeId:TID
) extends BasicThingInfo

case class PropValInfo(
  propInfo:PropInfo,
  prompt:Option[Wikitext],
  renderedV:Wikitext,
  tooltip:Option[Wikitext]
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
  displayName:String
) extends BasicThingInfo

case class AllTypeInfo(
  collections:Seq[CollectionInfo],
  standardTypes:Seq[TypeInfo],
  advancedTypes:Seq[TypeInfo],
  models:Seq[ThingInfo]
)
