package querki.time

import scala.xml.NodeSeq

import com.github.nscala_time.time.Imports._
import com.github.nscala_time.time.StaticDateTime

import models._

import querki.ecology._
import querki.globals._
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
  
  val Logic = initRequires[querki.logic.Logic]
  
  lazy val QDuration = interface[QDuration]
  lazy val QL = interface[querki.ql.QL]
    
  /******************************************
   * TYPES
   ******************************************/
  
  lazy val QDate = new SystemType[DateTime](DateTypeOID,
    toProps(
      setName("Date Type"),
      Categories(TimeTag),
      Summary("Represents a particular date"),
      Details("""A value of this Type indicates a specific date.
          |
          |You can create date literals in QL code, which is occasionally helpful; you do this by treating
          |`Date Type` as a function, and passing in a Text value to be parsed as a date, in the format
          |mm/dd/yyyy. So to create a constant for the first day of the 21st century, you would say:
          |```
          |Date Type(\""01/01/2000\"")
          |```
          |
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
    val defaultRenderFormat = DateTimeFormat.forPattern("MM/dd/yyyy")
    
    def doWikify(context:QLContext)(v:DateTime, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = {
      val formatter = displayOpt match {
        case Some(displayText) => DateTimeFormat.forPattern(displayText.plaintext)
        case None => defaultRenderFormat
      }
      Future.successful(Wikitext(formatter.print(v)))
    }

    /**
     * TODO: as usual, this is too incestuous with the actual implementation on the client side. We should be sending
     * more abstract information here, and having the client actually render the datepicker.
     */
    override def renderInputXml(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, v:ElemValue):Future[NodeSeq] = {
      val date = get(v)
      val str =
        if (date == epoch) {
          // Leave the field empty iff it's Optional
          // TODO: Uggggly. I think there's something fundamentally broken with Optional.doRenderInput, in that it's
          // forcing the default value in here even in the None case. Do we need separate concepts of "default" and "zero"?
          // Our doDefault here is really returning the zero (the epoch), but there is a *separate* concept of the default
          // (today).
          if (prop.cType == Optional)
            ""
          else
            doToUser(DateTime.now)(context.state)
        } else
          toUser(v)(context.state)
      fut(<input type="text" class="_dateInput" value={str}/>)
    }
    
    override def doComp(context:QLContext)(left:DateTime, right:DateTime):Boolean = { left < right } 
    override def doMatches(left:DateTime, right:DateTime):Boolean = { left.getMillis == right.getMillis }
    def doDefault(implicit state:SpaceState) = epoch
    
    def doComputeMemSize(v:DateTime):Int = 8
    
    def parseDate(str:String):QValue = {
      try {
        ExactlyOne(this(defaultRenderFormat.parseDateTime(str)))
      } catch {
        case ex:Exception => QL.ErrorValue(s"`str` is not a legal date -- please use the format mm/dd/yyyy")
      }
    }
    
    override def constructTypeValue(inv:Invocation):Option[QFut] = {
      val result:QFut = for {
        text <- inv.processParamFirstAs(0, Core.TextType)
      }
        yield parseDate(text.text)
        
      Some(result)
    }
  }
    
  class QDateTime(tid:OID) extends SystemType[DateTime](tid,
    toProps(
      setName("Date and Time Type"),
      Core.InternalProp(true),
      Categories(TimeTag),
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
      Future.successful(Wikitext(formatter.print(v)))
    }
    
    override def doComp(context:QLContext)(left:DateTime, right:DateTime):Boolean = { left < right } 
    override def doMatches(left:DateTime, right:DateTime):Boolean = { left.getMillis == right.getMillis }
    def doDefault(implicit state:SpaceState) = epoch
    def doComputeMemSize(v:DateTime):Int = 8
  }
  lazy val QDateTime = new QDateTime(DateTimeTypeOID)
  
  override lazy val types = 
    Seq(
      QDate,
      QDateTime
    )

  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val modTimeMethod = new InternalMethod(ModifiedTimeMethodOID, 
    toProps(
      setName("_modTime"), 
      Categories(TimeTag),
      Summary("When was this Thing last changed?"), 
      Details("""```
        |THING -> _modTime -> Date and Time
        |```
        |This method can receive any Thing; it produces the Date and Time when that Thing was last changed.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        thing <- inv.contextAllThings
      }
        yield ExactlyOne(QDateTime(thing.modTime))
    }
  }
  
  class DateExtractMethod(oid:OID, name:String, docName:String, f:DateTime => org.joda.time.DateTime.Property) extends InternalMethod(oid, 
    toProps(
      setName(name),
      Categories(TimeTag),
      Summary(s"Gets the $docName from a Date"),
      Details(s"""```
        |DATE -> $name -> WHOLE NUMBER
        |```""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        date <- inv.contextAllAs(QDate)
        result = f(date).get
      }
        yield ExactlyOne(IntType(result))
    }
  }
  
  lazy val yearMethod = new DateExtractMethod(YearMethodOID, "_year", "year", date => date.year)
  lazy val monthMethod = new DateExtractMethod(MonthMethodOID, "_month", "month", date => date.month)
  lazy val dayMethod = new DateExtractMethod(DayMethodOID, "_dayOfMonth", "day of the month", date => date.dayOfMonth)
  
  lazy val todayFunction = new InternalMethod(TodayFunctionOID,
    toProps(
      setName("_today"),
      Categories(TimeTag),
      Summary("Produces today's Date")))
  {
    override def qlApply(inv:Invocation):QFut = {
      Future.successful(ExactlyOne(QDate(DateTime.now)))
    }    
  }
  
  /**
   * You can apply _plus() to a QDate, with a Duration as the parameter.
   */
  lazy val plusDateImpl = new FunctionImpl(PlusDateImplOID, Logic.PlusMethod, Seq(QDate))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        date <- inv.contextAllAs(QDate)
        duration <- inv.processParamFirstAs(0, QDuration.DurationType)
        period = QDuration.toPeriod(duration, inv.state)
        result = date + period
      }
        yield ExactlyOne(QDate(result))
    }
  }
  
  lazy val CreateTimeFunction = new InternalMethod(CreateTimeFunctionOID,
    toProps(
      setName("_createTime"),
      Summary("Given a Thing, this produces the time when that Thing was created, if known"),
      Signature(
        expected = Some(Seq(LinkType), "A Thing"),
        reqs = Seq.empty,
        opts = Seq.empty,
        returns = (LinkType, "The time when that Thing was created, if known. This may be empty.")
      )))
  {
    override def qlApply(inv:Invocation):QFut = {
      implicit val s = inv.state
      for {
        t <- inv.contextAllThings
        time <- inv.opt(t.createTimeOpt)
      }
        yield ExactlyOne(QDateTime(time))
    }
  }
  
  override lazy val props = Seq(
    modTimeMethod,
    yearMethod,
    monthMethod,
    dayMethod,
    todayFunction,
    plusDateImpl,
    CreateTimeFunction
  )
}
