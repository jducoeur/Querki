package querki.data

import models.{Kind, Wikitext}

import querki.core.NameUtils

trait BasicThingInfo {
  def oid:String
  def linkName:Option[String]
  def displayName:String
  
  def urlName = linkName match {
    case Some(name) => NameUtils.toUrl(name)
    case None => oid
  }
}

/**
 * This is the API view of a Thing -- what it looks like on the wire, and in the Client.
 * Note that it is intentionally much less elaborate than the Server-side Thing.
 */
case class ThingInfo(
  oid:String, 
  linkName:Option[String], 
  displayName:String,
  modelOid:String,
  kind:Kind.Kind,
  isModel:Boolean,
  isEditable:Boolean,
  isDeleteable:Boolean,
  isInstantiatable:Boolean,
  isTag:Boolean,
  importedFrom:Option[SpaceInfo]) extends BasicThingInfo

case class SpaceInfo(
  oid:String, 
  linkName:Option[String], 
  displayName:String,
  ownerId:String,
  ownerHandle:String) extends BasicThingInfo

case class PropInfo(
  oid:String,
  linkName:Option[String],
  displayName:String,
  appliesTo:Option[Kind.Kind]
) extends BasicThingInfo

case class PropValInfo(
  propInfo:PropInfo,
  prompt:Option[Wikitext],
  renderedV:Wikitext,
  tooltip:Option[Wikitext]
)

case class SpaceProps(
  oid:String,
  linkName:Option[String],
  displayName:String,
  standardProps:Seq[PropInfo],
  advancedProps:Seq[PropInfo],
  apps:Seq[SpaceProps]
) extends BasicThingInfo

case class CollectionInfo(
  oid:String,
  linkName:Option[String],
  displayName:String
) extends BasicThingInfo

case class TypeInfo(
  oid:String,
  linkName:Option[String],
  displayName:String
) extends BasicThingInfo

case class AllTypeInfo(
  collections:Seq[CollectionInfo],
  standardTypes:Seq[TypeInfo],
  advancedTypes:Seq[TypeInfo],
  models:Seq[ThingInfo]
)
