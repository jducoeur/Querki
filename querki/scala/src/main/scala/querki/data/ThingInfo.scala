package querki.data

import models.Kind

/**
 * This is the API view of a Thing -- what it looks like on the wire, and in the Client.
 * Note that it is intentionally much less elaborate than the Server-side Thing.
 */
case class ThingInfo(
  oid:String, 
  linkName:Option[String], 
  displayName:String,
  kind:Kind.Kind,
  isEditable:Boolean,
  isDeleteable:Boolean)
{
}
