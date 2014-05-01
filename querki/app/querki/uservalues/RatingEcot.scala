package querki.uservalues

import scala.xml.NodeSeq

import models.{DisplayPropVal, Kind, Wikitext}

import querki.core.IntTypeBasis
import querki.ecology._
import querki.util.QLog
import querki.values.{ElemValue, QLContext, RequestContext}

object RatingMOIDs extends EcotIds(45) {
  val RatingTypeOID = moid(1)
  val RatingSummarizerOID = moid(2)  
  val LabelsPropOID = moid(3)
}

class RatingEcot(e:Ecology) extends QuerkiEcot(e) with IntTypeBasis with SummarizerDefs {
  import RatingMOIDs._
  
  val Basic = initRequires[querki.basic.Basic]
  val Types = initRequires[querki.types.Types]
  
  lazy val PlainTextType = Basic.PlainTextType
      
  /***********************************************
   * TYPES
   ***********************************************/
  
  lazy val RatingType = new IntTypeBase(RatingTypeOID,
    toProps(
      setName("Rating Type")))
  {
    override def renderInputXml(prop:Property[_,_], rc:RequestContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = {
      implicit val s = rc.state.get
      // Note that we are intentionally demanding a result here. If it's not defined, we expect to get LabelsProp's default.
      // So we don't expect this to ever be empty:
      val labels = prop.getProp(LabelsProp).rawList.map(_.text)
      <div class='rating' data-rating={get(v).toString} data-labels={labels.mkString(",")}></div>
    }
  }
  
  lazy val RatingSummarizer = new DiscreteSummarizer(RatingSummarizerOID, RatingType,
    toProps(
      setName("Rating Summarizer"),
      Summary("Given a User Value Property made of numbers (such as Ratings), this provides functions such as _average.")))
  {
	override def wikifyKey(context:QLContext, fromProp:Option[Property[_,_]], key:Int):Wikitext = {
	  implicit val state = context.state
	  fromProp match {
	    case Some(prop) => {
	      val labels = prop.getProp(LabelsProp).rawList.map(_.text)
	      val label = try {
	        labels(key)
	      } catch {
	        case ex:IndexOutOfBoundsException => key.toString
	      }
	      Wikitext(label)
	    }
	    case None => super.wikifyKey(context, fromProp, key)
	  }
	}     
  }

  override lazy val types = Seq(
    RatingType,
    RatingSummarizer
  )
      
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val LabelsProp = new SystemProperty(LabelsPropOID, PlainTextType, QList,
      toProps(
        setName("Chart Labels"),
        AppliesToKindProp(Kind.Property),
        SkillLevel(SkillLevelAdvanced),
        Summary("Gives the labels for each element of a Rating or Chart"),
        Details("""When used on a Rating, this gives the hover-text label for each of a stars, in order.
            |It will also be used to display the summary of the total of the User Ratings. The number of
            |Labels given dictates how many stars will be displayed.
            |
            |If a Rating Type Property does not have Chart Labels set on it, it will display five stars,
            |labeled "Poor", "Fair", "Good", "Great" and "Excellent".""".stripMargin),
        Types.DefaultValueProp(
            QList.makePropValue(Seq(
                PlainTextType("Poor"),
                PlainTextType("Fair"),
                PlainTextType("Good"),
                PlainTextType("Great"),
                PlainTextType("Excellent")), PlainTextType))))
  
  override lazy val props = Seq(
    LabelsProp
  )
}
