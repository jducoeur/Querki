package models

import com.github.nscala_time.time.Imports.DateTime

import Thing._

import ql.{QLPhrase}
import querki.util._
import querki.values.{QLContext, QValue, WikitextValue}

object MIMEType {
  type MIMEType = String
  
  val CSS = "text/css"
  val JPEG = "image/jpeg"
  val JSON = "application/json"
}

class Attachment(i:OID, s:OID, m:OID, pf: PropFetcher, mt:DateTime = modules.time.TimeModule.epoch) 
  extends ThingState(i, s, m, pf, mt, Kind.Attachment) 
{
  override def qlApply(context:QLContext, params:Option[Seq[QLPhrase]] = None):QValue = {
    // When you simply apply an Attachment, include that attachment
    // TODO: this is wrong -- it's assuming that all attachments are pictures. We really need a system
    // property that stores the MIMEType, so we can do this only iff it's appropriate, and do something
    // else for other kinds of attachments...
    val wikitext:Wikitext = Wikitext("![" + displayName + "](a/" + toThingId + ")")
    WikitextValue(wikitext)
  }
}

object Attachments {
}
