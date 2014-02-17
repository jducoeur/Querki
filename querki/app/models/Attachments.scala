package models

import Thing.PropFetcher

import querki.ecology.Ecology
import querki.ql.{Invocation, QLPhrase}
import querki.time.DateTime
import querki.util._
import querki.values.{QLContext, QValue}

object MIMEType {
  type MIMEType = String
  
  val CSS = "text/css"
  val JPEG = "image/jpeg"
  val JSON = "application/json"
  val CSV = "text/csv"
}

class Attachment(i:OID, s:OID, m:OID, pf: PropFetcher, mt:DateTime = querki.time.epoch)(implicit e:Ecology)  
  extends ThingState(i, s, m, pf, mt, Kind.Attachment)(e) 
{
  override def qlApply(inv:Invocation):QValue = {
    // When you simply apply an Attachment, include that attachment
    // TODO: this is wrong -- it's assuming that all attachments are pictures. We really need a system
    // property that stores the MIMEType, so we can do this only iff it's appropriate, and do something
    // else for other kinds of attachments...
    val wikitext:Wikitext = Wikitext("![" + displayName + "](a/" + toThingId + ")")
    interface[querki.ql.QL].WikitextValue(wikitext)
  }
}

object Attachments {
}
