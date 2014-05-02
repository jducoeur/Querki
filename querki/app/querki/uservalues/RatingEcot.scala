package querki.uservalues

import scala.xml.NodeSeq

import models.{DisplayPropVal, Kind, SimplePTypeBuilder, Wikitext}

import querki.core.IntTypeBasis
import querki.core.TypeUtils.DiscreteType
import querki.ecology._
import querki.util.QLog
import querki.values.{ElemValue, QLContext, RequestContext, SpaceState}

object RatingMOIDs extends EcotIds(45) {
  val RatingTypeOID = moid(1)
  val RatingSummarizerOID = moid(2)  
  val LabelsPropOID = moid(3)
  val RatingAverageTypeOID = moid(4)
  val AverageFunctionOID = moid(5)
}

class RatingEcot(e:Ecology) extends QuerkiEcot(e) with IntTypeBasis with SummarizerDefs with querki.core.MethodDefs {
  import RatingMOIDs._
  
  val Basic = initRequires[querki.basic.Basic]
  val Types = initRequires[querki.types.Types]
  
  lazy val PlainTextType = Basic.PlainTextType
      
  /***********************************************
   * TYPES
   ***********************************************/
  
  def getLabels(prop:Property[_,_])(implicit state:SpaceState) = prop.getProp(LabelsProp).rawList.map(_.text)
  
  lazy val RatingType = new IntTypeBase(RatingTypeOID,
    toProps(
      setName("Rating Type"))) with DiscreteType[Int]
  {
    override def renderInputXml(prop:Property[_,_], rc:RequestContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = {
      implicit val s = rc.state.get
      // Note that we are intentionally demanding a result here. If it's not defined, we expect to get LabelsProp's default.
      // So we don't expect this to ever be empty:
      val labels = getLabels(prop)
      <div class='_rating' data-rating={get(v).toString} data-labels={labels.mkString(",")}></div>
    }
    
    /**
     * The Range of a Rating Property is based on the number of Labels available.
     */
    def range(prop:Property[Int,_])(implicit state:SpaceState) = {
      val nLabels = prop.getProp(LabelsProp).v.cv.size
      1 until (nLabels+1)
    }
    
    override def editorSpan(prop:Property[_,_]) = 2
  }
  
  case class RatingAverage(propId:OID, avg:Double)
  
  /**
   * At least for now, rating averages get their own Type, because they display differently from other
   * floats. This likely should get refactored eventually, but it'll do to start.
   */
  lazy val RatingAverageType = new SystemType[RatingAverage](RatingAverageTypeOID,
    toProps(
      setName("_ratingAverageType"),
      setInternal)) with SimplePTypeBuilder[RatingAverage]
  {
    def doWikify(context:QLContext)(v:RatingAverage, displayOpt:Option[Wikitext] = None) = {
      implicit val state = context.state
      state.prop(v.propId) match {
        case Some(prop) => {
          val labels = getLabels(prop)
          Wikitext(s"""<div class='_rating' data-rating='${"%.2f" format v.avg}' data-labels='${labels.mkString(",")}' data-readonly='true'></div> 
          |<span class="_ratingAvg">${"%.2f" format v.avg}</span>""".stripMargin)
        }
        case None => {
          QLog.warn(s"_ratingAverageType called on unknown Property ${v.propId}")
          Wikitext(s"""<div class='_rating' data-rating='${"%.2f" format v.avg}' data-readonly='true'></div>
          |<span class="_ratingAvg">${"%.2f" format v.avg}</span>""".stripMargin)
        }
      }
    }
    
    // None of these should be possible, so for now I'm not going to worry about them:
    def doDeserialize(v:String)(implicit state:SpaceState) = ???
    def doSerialize(v:RatingAverage)(implicit state:SpaceState) = ???
    def doDefault(implicit state:SpaceState) = ???
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
	      val labels = getLabels(prop)
	      val label = try {
	        // The Star ratings run from 1-n, so we need to adjust for the 0-based labels list:
	        labels(key - 1)
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
   * FUNCTIONS
   ***********************************************/
  
  lazy val AverageFunction = new InternalMethod(AverageFunctionOID,
    toProps(
      setName("_average"),
      Summary("Calculate the average of some Ratings"),
      Details("""When you have a Rating or Review Property, you often want to know the average value
          |of those ratings. This function is how you get those.
          |
          |To use _average, your User Value Property must define the Summary Link Property, and set that
          |to a Property of Rating Summarizer type. That Summarizer keeps track of the statistics about
          |those User Values, and you can use the _average function on it.
          |
          |So for example, say that you have a Property called My Ratings, which is of Rating Type. You should
          |create another Property named My Rating Summary with the type Rating Summarizer, and set My Ratings' Summary Link to that.
          |Once you have done that, you can say `\[[My Rating Summary -> _average\]]` to show the average rating of this
          |Thing.
          |
          |The average will display with stars, much like the input for Ratings. It is of a special Type named
          |_ratingAverageType, which is only used for this purpose.
          |
          |This Function will eventually be broadened, to let you calculate averages of Lists and Sets of numbers more
          |generally. If you need that, please speak up, and we'll prioritize it.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      for {
        summary <- inv.contextAllAs(RatingSummarizer)
        avg = calcAverage(summary.content.toSeq)
      }
        yield ExactlyOne(RatingAverageType(RatingAverage(summary.propId, avg)))
    }
    
    def calcAverage(pairs:Seq[(Int, Int)]):Double = {
      val (sum, n) = ((0, 0) /: pairs) { (accum, pair) =>
        val (curTotal, curEntries) = accum
        val (key, numEntries) = pair
        (curTotal + (key * numEntries), curEntries + numEntries)
      }
      sum.toDouble / n.toDouble
    }
  }
      
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
    AverageFunction,
      
    LabelsProp
  )
}
