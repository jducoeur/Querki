package querki.time

import com.github.nscala_time.time.Imports._

import models._

import Thing._

import querki.ecology._
import querki.values.QLContext

/**
 * The TimeModule is responsible for all things Time-related in the Querki API.
 * 
 * Conceptually, it is a fairly thin layer over Joda-time -- Joda is pretty clean and
 * well-built, so we may as well adapt from it. When in doubt, use Joda's conceptual
 * structure.
 * 
 * As of this writing, the methods are all in System, but that should probably change:
 * I expect us to expose a *lot* of methods and types through this Module. So in the
 * medium term, most of it should become an opt-in Mixin, with only the most essential
 * methods in System. (This will likely require us to split this Module into two, which
 * should be fine.) 
 */
class TimeModule(e:Ecology) extends QuerkiEcot(e) with Time with querki.core.MethodDefs {
 
  import MOIDs._
    
  /******************************************
   * TYPES
   ******************************************/
  
  class QDateTime(tid:OID) extends SystemType[DateTime](tid,
      toProps(
        setName("Date and Time Type")
      )) with SimplePTypeBuilder[DateTime]
  {
    def doDeserialize(v:String) = new DateTime(v.toLong)
    def doSerialize(v:DateTime) = v.millis.toString
    val defaultRenderFormat = DateTimeFormat.mediumDateTime
    
    def doWikify(context:QLContext)(v:DateTime, displayOpt:Option[Wikitext] = None) = {
      val formatter = displayOpt match {
        case Some(displayText) => DateTimeFormat.forPattern(displayText.plaintext)
        case None => defaultRenderFormat
      }
      Wikitext(formatter.print(v))
    }
    
    override def doComp(context:QLContext)(left:DateTime, right:DateTime):Boolean = { left < right } 
    override def doMatches(left:DateTime, right:DateTime):Boolean = { left.millis == right.millis }
    val doDefault = epoch
  }
  lazy val QDateTime = new QDateTime(DateTimeTypeOID)
  override lazy val types = Seq(QDateTime)

  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val modTimeMethod = new SingleThingMethod(ModifiedTimeMethodOID, "_modTime", "When was this Thing last changed?", 
      """THING -> _modTime -> Date and Time
      |This method can receive any Thing; it produces the Date and Time when that Thing was last changed.""",
      {(t:Thing, _:QLContext) => ExactlyOne(QDateTime(t.modTime)) })

  override lazy val props = Seq(
    modTimeMethod
  )
}