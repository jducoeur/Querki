package querki.uservalues

import scala.xml.NodeSeq

import models.{DisplayPropVal, Kind, SimplePTypeBuilder, ThingState, Wikitext}

import querki.core.IntTypeBasis
import querki.core.TypeUtils.DiscreteType
import querki.ecology._
import querki.types.{ModeledPropertyBundle, ModelTypeDefiner}
import querki.util.QLog
import querki.values.{ElemValue, QLContext, RequestContext, SpaceState}

object RatingMOIDs extends EcotIds(45) {
  val RatingTypeOID = moid(1)
  val RatingSummarizerOID = moid(2)  
  val LabelsPropOID = moid(3)
  val RatingAverageTypeOID = moid(4)
  val AverageFunctionOID = moid(5)
  val RatingPropOID = moid(6)
  val RatingSummaryPropOID = moid(7)
  val ReviewModelOID = moid(8)
  val ReviewPropOID = moid(9)
  val ReviewCommentsPropOID = moid(10)
  val ReviewTypeOID = moid(11)
  val RatingShowTargetOID = moid(12)
}

class RatingEcot(e:Ecology) extends QuerkiEcot(e) with IntTypeBasis with SummarizerDefs with querki.core.MethodDefs with ModelTypeDefiner {
  import RatingMOIDs._
  
  val Basic = initRequires[querki.basic.Basic]
  val Editor = initRequires[querki.editing.Editor]
  val Types = initRequires[querki.types.Types]
  val UserValuesRaw = initRequires[UserValues]
  
  lazy val UserValues:UserValues = UserValuesRaw
  
  lazy val IsUserValueFlag = UserValues.IsUserValueFlag
  lazy val PlainTextType = Basic.PlainTextType
  lazy val SummaryLink = UserValues.SummaryLink
      
  /***********************************************
   * MODELS
   ***********************************************/
  
  lazy val ReviewModel = ThingState(ReviewModelOID, systemOID, RootOID,
    toProps(
      setName("_reviewModel"),
      setInternal,
      RatingProperty(),
      ReviewCommentsProperty(),
      Editor.InstanceProps(RatingProperty, ReviewCommentsProperty)))
      
  override lazy val things = Seq(
    ReviewModel
  )
  
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
      
      if (prop.ifSet(RatingShowTargetProperty)) {
        val targetId = "target" + currentValue.suffix
        // HACK: the _ratingTargetWrapper is there because *both* of the top-level nodes will get their ids set by HtmlRenderer. Do we
        // have any good way of preventing it from doing so to the _ratingTarget?
        <div class='_rating' data-rating={get(v).toString} data-labels={labels.mkString(",")} data-target={targetId} data-targetkeep='true'></div> :+
        <div class='_ratingTargetWrapper'><div id={targetId} class='_ratingTarget'></div></div>
        
      } else
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
    
    // TODO: this really ought to include the appropriate labels for this value, but we don't know the Property!
    // Do we need to change this Type to include the PropId? Is that even feasible?
    override def doWikify(context:QLContext)(v:Int, displayOpt:Option[Wikitext] = None) = {
      implicit val state = context.state
      Wikitext(s"""<div class='_rating' data-rating='$v' data-readonly='true'></div>""".stripMargin)
    }
  }
  
  lazy val ReviewType = new ModelType(ReviewTypeOID, ReviewModelOID,
    toProps(
      setName("Review Type"),
      // We want to display this in the Editor, even though it is a Model Type:
      Basic.ExplicitProp(true)))
  {
    override def doWikify(context:QLContext)(bundle:ModeledPropertyBundle, displayOpt:Option[Wikitext]) = {
      implicit val state = context.state
      val result = for {
        ratingPV <- bundle.getPropOpt(RatingProperty)
        ratingRendered = ratingPV.v.wikify(context, displayOpt)
        commentsPV <- bundle.getPropOpt(ReviewCommentsProperty)
        commentsRendered = commentsPV.v.wikify(context, displayOpt)
      }
        yield ratingRendered + Wikitext(" -- ") + commentsRendered
        
      result.getOrElse(Wikitext.empty)
    }
  }
  
  case class RatingAverage(propId:OID, avg:Double, n:Int)
  
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
          |<span class="_ratingAvg">${"%.2f" format v.avg} (${v.n})</span>""".stripMargin)
        }
        case None => {
          QLog.warn(s"_ratingAverageType called on unknown Property ${v.propId}")
          Wikitext(s"""<div class='_rating' data-rating='${"%.2f" format v.avg}' data-readonly='true'></div>
          |<span class="_ratingAvg">${"%.2f" format v.avg} (${v.n})</span>""".stripMargin)
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
    RatingSummarizer,
    ReviewType
  )
  
  /***********************************************
   * FUNCTIONS
   ***********************************************/
  
  /**
   * TBD: this probably doesn't belong here -- in the long run, we probably should have a
   * querki.statistics namespace. But this will do for now: since it's just a standard Function,
   * we can change the OID later without any likely consequences.
   */
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
        n = summary.content.size
      }
        yield ExactlyOne(RatingAverageType(RatingAverage(summary.propId, avg, n)))
    }
    
    def calcAverage(pairs:Seq[(Int, Int)]):Double = {
      val (sum, n) = ((0, 0) /: pairs) { (accum, pair) =>
        val (curTotal, curEntries) = accum
        val (key, numEntries) = pair
        (curTotal + (key * numEntries), curEntries + numEntries)
      }
      n match {
        case 0 => 0
        case _ => sum.toDouble / n.toDouble
      }
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
  
  lazy val RatingSummaryProperty = new SystemProperty(RatingSummaryPropOID, RatingSummarizer, ExactlyOne,
      toProps(
        setName("Rating Summary"),
        // Need to declare this the hard way, to avoid initialization loops. This needs
        // to be declared here so that the summary will work for Review as well as Rating.
        (MOIDs.SummarizesPropertyLinkOID -> ExactlyOne(LinkType(RatingPropOID))),
        Summary("Collects the ratings from the Rating or Review Property"),
        Details("""If you have the Rating or Review Property on a Model or Thing, it will wind
            |up also adding Rating Summary (after someone actually gives a Rating).
            |
            |You can say `\[[Rating Summary\]]` to display a small bar chart
            |of how many people have given each rating, or `\[[Rating Summary -> _average\]]`
            |to show the overall average of the ratings.""".stripMargin)))
  
  lazy val RatingProperty = new SystemProperty(RatingPropOID, RatingType, ExactlyOne,
      toProps(
        setName("Rating"),
        IsUserValueFlag(true),
        SummaryLink(RatingSummaryProperty),
        Editor.PromptProp(Core.QNone),
        Summary("Allows many people to rate how much they like this Thing"),
        Details("""If you put this Property on a Thing or Model, you can simply say
            |`\[[Rating._edit\]]` in your Default View, and it will allow you to rate
            |how much you like this Thing, as 1-5 stars.
            |
            |Rating is a User Value Property -- each Member can give their own answer.
            |
            |By adding this Property, you will also wind up adding the Rating Summary Property
            |on each Thing. You can say `\[[Rating Summary\]]` to display a small bar chart
            |of how many people have given each rating, or `\[[Rating Summary -> _average\]]`
            |to show the overall average of the ratings.
            |
            |If you want to let people give a comment with their rating, use the Review Property
            |instead -- that combines Rating with a comment field. (Do not use Rating and Review
            |on the same Thing -- it will cause confusion.)
            |
            |If you want to do more sophisticated things (for instance, having multiple different
            |Ratings or Reviews for different purposes on the same Thing), you can define your
            |own Properties of Rating Type and Rating Summarizer. However, this is fairly complicated,
            |and is only for advanced users for the time being. For most purposes, the built in
            |Rating and Review Properties should do fine.""".stripMargin)))
  
  lazy val ReviewCommentsProperty = new SystemProperty(ReviewCommentsPropOID, LargeTextType, ExactlyOne,
      toProps(
        setName("Review Comments"),
        setInternal,
        Editor.PromptProp("Comments:"),
        Summary("The text part of a Review. Not intended for use on its own.")))
  
  lazy val ReviewProperty = new SystemProperty(ReviewPropOID, ReviewType, ExactlyOne,
      toProps(
        setName("Review"),
        IsUserValueFlag(true),
        SummaryLink(RatingSummaryProperty),
        Summary("Allows people to provide their own reviews of this Thing."),
        Details("""If you put this Property on a Thing or Model, you can simply say
            |`\[[Review._edit\]]` in your Default View, and it will allow you to provide
            |a Rating and a comment about this Thing.
            |
            |Review is a User Value Property -- each Member can give their own answer.
            |
            |By adding this Property, you will also wind up adding the Rating Summary Property
            |on each Thing. You can say `\[[Rating Summary\]]` to display a small bar chart
            |of how many people have given each rating, or `\[[Rating Summary -> _average\]]`
            |to show the overall average of the ratings.
            |
            |If you just want a simple 1-5 star rating, use the Rating Property instead. (Do not use Rating and Review
            |on the same Thing -- it will cause confusion.)
            |
            |If you want to do more sophisticated things (for instance, having multiple different
            |Ratings or Reviews for different purposes on the same Thing), you can define your
            |own Properties of Rating Type and Rating Summarizer. However, this is fairly complicated,
            |and is only for advanced users for the time being. For most purposes, the built in
            |Rating and Review Properties should do fine.""".stripMargin)))
  
  lazy val RatingShowTargetProperty = new SystemProperty(RatingShowTargetOID, YesNoType, ExactlyOne,
    toProps(
      setName("Show External Rating Hint"),
      SkillLevel(SkillLevelAdvanced),
      Summary("Displays the current Rating Hint in its own text field, on the side."),
      Details("""Every Rating Property includes "hints", defined by the Chart Labels property -- basically text
          |descriptions of what each star means, such as "Poor" or "Excellent". Normally, these are shown as hover
          |text, displayed when you pause over a star for a second. If you would instead prefer to show the hint
          |immediately, next to the stars, add this to your Rating Property and set it to true.""".stripMargin)))
  
  override lazy val props = Seq(
    AverageFunction,
      
    LabelsProp,
    RatingSummaryProperty,
    RatingProperty,
    ReviewCommentsProperty,
    ReviewProperty,
    RatingShowTargetProperty
  )
}
