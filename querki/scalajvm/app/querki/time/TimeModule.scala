package querki.time

import scala.xml.NodeSeq

import com.github.nscala_time.time.Imports._

import models._

import querki.ecology._
import querki.values.{ElemValue, QLContext, SpaceState}

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
  
  lazy val QDate = new SystemType[DateTime](DateTypeOID,
      toProps(
        setName("Date Type"),
        Summary("Represents a particular date"),
        Details("""A value of this Type indicates a specific date.
            |ADVANCED: under the hood, DateTimes are based on the [Joda-Time](http://www.joda.org/joda-time/)
            |library, and you can use Joda-Time format strings when displaying a Date. For example,
            |if you say:
            |```
            |\[[My Thing -> My Date Property -> \""\__MMM dd 'yy\__\""\]]
            |```
            |it will display as something like "Mar 10 '96".
            |
            |In the long time, we will probably add functions corresponding to most of the capabilities
            |of Joda-Time. If there are specific functions or features that you need, please ask for them.""".stripMargin)))
     with SimplePTypeBuilder[DateTime]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = new DateTime(v.toLong)
    def doSerialize(v:DateTime)(implicit state:SpaceState) = v.getMillis().toString
    override def doToUser(v:DateTime)(implicit state:SpaceState):String = defaultRenderFormat.print(v)
    val defaultRenderFormat = DateTimeFormat.forPattern("MM-dd-yyyy")
    
    def doWikify(context:QLContext)(v:DateTime, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = {
      val formatter = displayOpt match {
        case Some(displayText) => DateTimeFormat.forPattern(displayText.plaintext)
        case None => defaultRenderFormat
      }
      Wikitext(formatter.print(v))
    }
    
    override def renderInputXml(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = {
      val str = toUser(v)(context.state)
      <input type="text" class="_dateInput" value={str}/>
    }
    
    override def doComp(context:QLContext)(left:DateTime, right:DateTime):Boolean = { left < right } 
    override def doMatches(left:DateTime, right:DateTime):Boolean = { left.getMillis == right.getMillis }
    def doDefault(implicit state:SpaceState) = epoch    
  }
  
  class QDateTime(tid:OID) extends SystemType[DateTime](tid,
      toProps(
        setName("Date and Time Type"),
        Core.InternalProp(true),
        Summary("Represents a particular date and time"),
        Details("""A value of this Type indicates a specific moment in time.
            |
            |At the moment, the only way to get a DateTime is through the _modTime function, which
            |produces the DateTime when the given Thing was last changed.
            |
            |ADVANCED: under the hood, DateTimes are based on the [Joda-Time](http://www.joda.org/joda-time/)
            |library, and you can use Joda-Time format strings when displaying a DateTime. For example,
            |if you say:
            |```
            |\[[My Thing -> _modTime -> \""\__KK:mm MMM dd 'yy\__\""\]]
            |```
            |it will display as something like "09:24 Mar 10 '96".
            |
            |In the long time, we will probably add functions corresponding to most of the capabilities
            |of Joda-Time. If there are specific functions or features that you need, please ask for them.
            |
            |There is no UI for entering DateTime values yet. (At least, not one that real people
            |can use.) For that reason, Date and Time Type is currently marked as Internal -- you can't
            |create a Data and Time Property yet. This will change in the future: if DateTimes are
            |important to your use case, please say so, and we may prioritize it higher.""".stripMargin)
      )) with SimplePTypeBuilder[DateTime]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = new DateTime(v.toLong)
    def doSerialize(v:DateTime)(implicit state:SpaceState) = v.getMillis().toString
    val defaultRenderFormat = DateTimeFormat.mediumDateTime
    
    def doWikify(context:QLContext)(v:DateTime, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = {
      val formatter = displayOpt match {
        case Some(displayText) => DateTimeFormat.forPattern(displayText.plaintext)
        case None => defaultRenderFormat
      }
      Wikitext(formatter.print(v))
    }
    
    override def doComp(context:QLContext)(left:DateTime, right:DateTime):Boolean = { left < right } 
    override def doMatches(left:DateTime, right:DateTime):Boolean = { left.getMillis == right.getMillis }
    def doDefault(implicit state:SpaceState) = epoch
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