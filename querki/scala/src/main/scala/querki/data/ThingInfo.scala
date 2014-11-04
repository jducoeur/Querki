package querki.data

import models.{Kind, Wikitext}

import querki.core.NameUtils

trait Urlable {
  def oid:String
  def linkName:Option[String]
  
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
  importedFrom:Option[SpaceInfo]) extends Urlable
{
}

case class SpaceInfo(
  oid:String, 
  linkName:Option[String], 
  displayName:String,
  ownerId:String,
  ownerHandle:String) extends Urlable
{
}

case class PropValInfo(
  name:String,
  prompt:Option[Wikitext],
  renderedV:Wikitext,
  tooltip:Option[Wikitext]
)
