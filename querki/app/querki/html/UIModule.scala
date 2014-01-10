package querki.html

import scala.xml._

import play.api.templates.Html

import models.{HtmlWikitext, OID, QWikitext, Wikitext}
import models.system.{ExternalLinkType, URLableType}

import ql.QLPhrase

import querki.ecology._
import querki.util._
import querki.values._

object UIMOIDs extends EcotIds(11) {
  val SectionMethodOID = sysId(43)
  val LinkButtonOID = sysId(51)
  val IconButtonOID = sysId(68)
  val CreateInstanceLinkOID = sysId(69)
  val ShowLinkMethodOID = sysId(95)
  val PropLinkMethodOID = sysId(96)
  
  val ClassMethodOID = moid(1)
  val TooltipMethodOID = moid(2)
}

class UIModule(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs {
  import UIMOIDs._

  lazy val HtmlRenderer = interface[querki.html.HtmlRenderer]
  lazy val QL = interface[querki.ql.QL]

  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  /**
   * This is the abstraction of a single-parameter function that takes some HTML and modifies it. It is
   * mainly intended for use with functions that change the attributes of the HTML.
   */
  abstract class HtmlModifier(oid:OID, name:String, summary:String, details:String) extends InternalMethod(oid, 
    toProps(
      setName(name),
      Summary(summary),
      Details(details))) 
  {
    // Actual Modifier classes should implement this, which does the heart of the work
    def doTransform(elem:Elem, paramText:String):Elem
    
    override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
      val v = context.value
      if (v.pType != RawHtmlType && v.pType != ParsedTextType)
        throw new PublicException("UI.transform.htmlRequired", name)
      if (paramsOpt.isEmpty)
        throw new PublicException("UI.transform.classRequired", name)
      val params = paramsOpt.get

      def processHtml(html:Html):HtmlWikitext = {
        val parsedParamOpt = context.parser.get.processPhrase(params(0).ops, context).value.firstTyped(ParsedTextType)
        if (parsedParamOpt.isEmpty)
          throw new PublicException("UI.transform.classRequired", name)
        val paramText = parsedParamOpt.get.raw.toString
        // TODO: It is truly annoying that I have to handle things this way -- there should be a
        // cleaner way to deal. I begin to suspect that I should be passing around XML instead
        // of raw HTML, so I can manipulate it better.
        val rawHtml = html.body
        val rawXml = XML.loadString(rawHtml)
        val newXml = doTransform(rawXml, paramText)
        val newHtml = Html(Xhtml.toXhtml(newXml))
        HtmlWikitext(newHtml)        
      }
      
      try {
	      v.pType match {
	        case RawHtmlType => {
		      v.map(RawHtmlType, RawHtmlType) { wikitext =>
		        wikitext match {
		          case HtmlWikitext(html) => processHtml(html)
		        }
		      }          
	        }
	        
	        case ParsedTextType => {
		      v.map(ParsedTextType, RawHtmlType) { wikitext => processHtml(wikitext.span.html) }          
	        }
	      }
      } catch {
        case ex:org.xml.sax.SAXParseException => throw new PublicException("UI.transform.notWellFormed", name)
      }
    }
  }
  
  lazy val classMethod = new HtmlModifier(ClassMethodOID, "_class",
      "Add a class tag to the received HTML value",
      """Usually, to add HTML classes to something (to make them look pretty via CSS), you use the
            |\{\{class:...\}\} mechanism. But that *wraps* the text, inside of a div or span, and while that is
            |usually good enough, sometimes it doesn't do everything you need. So _class provides an alternate way
            |to do this via QL -- given an HTML block, this adds the class to *that* block, instead of wrapping it
            |in another.
            |
            |This is mainly intended for use with _edit, to do something like this:
            |[[_code(""[[My Thing -> My Prop._edit -> _class(""myClass"")]]"")]]
            |This will create an Edit input for My Prop, with myClass attached so you can control its display.
            |
            |This can also be used to add a class to a given text block:
            |[[_code(""[[""Hello world"" -> _class(""myClass"")]]"")]]
            |This will create a paragraph for "hello world" as usual, but will attach "myClass" as a class on that
            |paragraph. (This is less often necessary, but occasionally helpful.)""".stripMargin)
  {
    def doTransform(elem:Elem, paramText:String):Elem = HtmlRenderer.addClasses(elem, paramText)
  }
  
  lazy val tooltipMethod = new HtmlModifier(TooltipMethodOID, "_tooltip",
      "Add a tooltip to the received HTML value",
      """When you have a title, or some other short text like that, you sometimes want a "tooltip" -- a little
      |pop-up -- with a better description of what it means. This function lets you add that.
      |
      |Since _tooltip is a function, you have to use it inside a QL expression, like this:
      |[[_code(""[[""My Thing"" -> _tooltip(""My Thing is a special sort of Thing"")]]"")]]
      |In the long run, you will be able to describe a tooltip without using a QL expression, but
      |for now, this is the way to do it.""".stripMargin)
  {
    def doTransform(elem:Elem, paramText:String):Elem = {
      val withClass = HtmlRenderer.addClasses(elem, "_withTooltip")
      withClass % Attribute("title", Text(paramText), Null)
    }
  }

  /***********************************************
   * FUNCTIONS
   ***********************************************/

  class SectionMethod extends InternalMethod(SectionMethodOID,
    toProps(
      setName("_section"),
      Summary("Display a List as a Header, followed by its contents"),
      Details("""_section is intended for the common case where you want to display a section
          |on the page if and only if a specific List is non-empty. It looks like this:
          |    My List -> _section(HEADER, DETAILS, EMPTY)
          |Each of the parameters can be any QL phrase, although they are often just text blocks. They are
          |treated as follows:
          |
          |* HEADER is shown first, if the incoming List is non-empty. It gets the entire List as its Context.
          |* DETAILS is shown after the header. It is repeated for each element in the List, just as it would
          |if you fed a List into a normal text block.
          |* EMPTY is shown if and only if the List is empty. This lets you show something else if appropriate.
          |It is optional -- you can leave it off.
          |
          |Note that the generated QText will have the HEADER on a separate line from the DETAILS. This is
          |usually what you want. It means that, for example, if you start the HEADER with "###", it will show
          |up as a true header, separate from the DETAILS, but if it is just something like "Contents:", the
          |HEADER and DETAILS will run together, since QText joins ordinary text lines together.""".stripMargin)
    )) 
  {
  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
    paramsOpt match {
      case Some(params) if (params.length > 0) => {
        val header = params(0)
        val details = if (params.length > 1) Some(params(1)) else None
        val empty = if (params.length > 2) Some(params(2)) else None
        buildSection(context, header, details, empty)
      }
      case _ => QL.ErrorValue("_section requires at least one parameter")
    }
  }  
  
  def buildSection(context:QLContext, header:QLPhrase, detailsOpt:Option[QLPhrase], emptyOpt:Option[QLPhrase]):QValue = {
    val parser = context.parser.get
    val wikitext = if (context.isEmpty) {
      parser.contextsToWikitext(emptyOpt.map(empty => Seq(parser.processPhrase(empty.ops, context.root))).getOrElse(Seq.empty))
    } else {
      val processedHeader = parser.contextsToWikitext(Seq(parser.processPhrase(header.ops, context.asCollection)))
      val processedDetails = detailsOpt.map(details => Seq(parser.processPhrase(details.ops, context)))
      // TODO: why are we transforming this to Wikitext this early? Is there any reason to? Shouldn't we just turn all
      // of this into a new List Context and pass it on through? Conceptually that would be more correct. The only problem
      // is that the Header and Details potentially produce different Types, so they might not fit neatly into a single List.
      // Which implies, of course, that what we *should* be producing here is a Tuple of (Header, List[Details]). Hmm --
      // let's revisit this once we have Tuples implemented.
      processedDetails match {
        // Note that there is automatically a newline inserted between the Header and Details. Most of the time, this
        // produces exactly the right result:
        case Some(details) => processedHeader + parser.contextsToWikitext(details, true)
        case None => processedHeader
      }
    }
    WikitextValue(wikitext)
  }
  }

	abstract class ButtonBase(tid:OID, pf:PropFetcher) extends InternalMethod(tid, pf)
	{
	  def generateButton(url:String, params:Seq[Wikitext]):scala.xml.Elem
	  
	  def numParams:Int
	  
	  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
	    paramsOpt match {
	      case Some(params) if (params.length == numParams) => {
	        val urlOpt = context.value.pType match {
	          case pt:URLableType => context.value.firstOpt.flatMap(pt.getURL(context)(_))
	          case _ => None
	        }
	        
	        urlOpt match {
	          case Some(url) => {
	            val paramTexts = params.map(phrase => context.parser.get.processPhrase(phrase.ops, context).value.wikify(context))
	            HtmlValue(Html(generateButton(url, paramTexts).toString))            
	          }
	          // Nothing incoming, so cut.
	          // TODO: there is probably a general pattern to pull out here, of "cut processing if the input is empty"
	          case None => EmptyValue(RawHtmlType)
	        }
	      }
	      case None => WarningValue(displayName + " requires " + numParams + " parameters.")
	    }
	  }
	}

	class LinkButtonMethod extends ButtonBase(LinkButtonOID,
	    toProps(
	      setName("_linkButton"),
	      Summary("Displays a button that goes to a linked page when you press it."),
	      Details("""    LINK -> _linkButton(LABEL)
	          |_linkButton receives a Link or External Link, and displays that
	          |link as a button. It expects one parameter, which will be the label of the button.""".stripMargin)))
	{
	  val numParams = 1
	  
	  def generateButton(url:String, params:Seq[Wikitext]):scala.xml.Elem = {
	    <a class="btn btn-primary" href={url}>{params(0).raw}</a>
	  }
	}
	
	class IconButtonMethod extends ButtonBase(IconButtonOID,
	    toProps(
	      setName("_iconButton"),
	      Summary("Displays a button showing an icon, that goes to a linked page when you press it."),
	      Details("""    LINK -> _iconButton(ICON, TOOLTIP)
	          |_iconButton receives a Link or External Link, and displays that
	          |link as a button. The first parameter identifies the icon to use for the button; the second is the
	          |hover text to display as a tooltip. Both parameters are required.
	          |
	          |For icons, you may use anything from the [Bootstrap Glyphicon](http://getbootstrap.com/2.3.2/base-css.html#icons) set.
	          |Just use the name of the icon (in double-double quotes) in the parameter.""".stripMargin)))
	  {
	  val numParams = 2
	  
	  def generateButton(url:String, params:Seq[Wikitext]):scala.xml.Elem = {
	    <a class="btn btn-mini btn-primary" href={url} title={params(1).raw}><i class={params(0).raw + " icon-white"}></i></a>
	  }
	}
	
	// TODO: this is very similar to _linkButton, and should be refactored.
	class ShowLinkMethod extends InternalMethod(ShowLinkMethodOID,
	    toProps(
	      setName("_showLink"),
	      Summary("Displays a Link or External Link as a normal HTML link."),
	      Details("""    LINK -> _showLink(LABEL)
	          |This is the most normal way to display a Link or External Link with a chosen label. The
	          |label may be any expression you choose.
	          |
	          |The default behaviour of a Link, if you don't do anything with it, is effectively
	          |"_showLink(Default View)".""".stripMargin)))
	{
	  override def qlApply(context:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue = {
	    paramsOpt match {
	      case Some(params) if (params.length > 0) => {
	        context.value.pType match {
	          case pt:URLableType => {
	            context.collect(ParsedTextType) { elemContext =>
	              val wikitextOpt = for (
	                elemV <- elemContext.value.firstOpt;
	                url <- pt.getURL(elemContext)(elemV);
	                label = elemContext.parser.get.processPhrase(params(0).ops, elemContext).value.wikify(elemContext)
	                  )
	                yield QWikitext("[") + label + QWikitext(s"]($url)")
	              
	              wikitextOpt match {
	                case Some(wikitext) => QValue.make(ExactlyOne, ParsedTextType, wikitext)
	                case None => Core.emptyListOf(ParsedTextType)
	              }
	            }
	          }
	          case _ => WarningValue(displayName + " can only be used with Link types")
	        }
	      }
	      case None => WarningValue(displayName + " requires a label parameter.")
	    }
	  }
	}
		
	class PropLinkMethod extends ThingPropMethod(PropLinkMethodOID, 
	    toProps(
	      setName("_propLink"),
	      Summary("""Produces a Link to a specific Property on a Thing."""),
	      Details("""    THING -> PROPERTY._propLink -> EXTERNAL LINK
	          |A common pattern in Querki is to provide alternate "views" for a Thing -- different ways of displaying it.
	          |Typically, you do this by creating another Large Text Property (separate from Default View), which contains
	          |the alternate view, and then linking to that somewhere. This method makes it easy to do so: feed the THING
	          |and PROPERTY into _propLink, and the result is an EXTERNAL LINK which you can then pass to _showLink,
	          |_linkButton or _iconButton.
	          |
	          |NOTE: this currently only works for Things in the local Space, and probably does *not* work correctly in
	          |sub-Spaces yet.
	          |
	          |NOTE: this does not check that the specified PROPERTY is actually a Text Property, so be careful!""".stripMargin)))
	{
	  def applyToPropAndThing(mainContext:QLContext, mainThing:Thing, 
	    partialContext:QLContext, propErased:Property[_,_],
	    params:Option[Seq[QLPhrase]]):QValue =
	  {
	    ExactlyOne(ExternalLinkType(mainThing.toThingId + "?prop=" + propErased.toThingId))
	  }
	}
	
	// TODO: this is so full of abstraction breaks it isn't funny. Using routes here is inappropriate; indeed, the fact that we're referring
	// to Play at all in this level is inappropriate. This probably needs to be routed through the rendering system, so that it takes the
	// current rendering environment and produces a relative control appropriate within it. But it'll do for the short term.
	import controllers.routes
	class CreateInstanceLinkMethod extends SingleThingMethod(CreateInstanceLinkOID, "_createInstanceLink", 
	    "Given a received Model, this produces a Link to create an instance of that Model.",
	    """    MODEL -> _createInstanceLink -> _linkButton(LABEL)
	    |This is how you implement a "Create" button. _createInstanceLink takes a MODEL, and produces an External Link to the page to create a new Instance of it.
	    |
	    |You will usually then feed this into, eg, _linkButton or _iconButton as a way to display the Link.""".stripMargin,
	{ (thing, context) => 
	  import controllers.PlayRequestContext
	  context.request match {
	    case PlayRequestContext(request, _, _, _, _, _, _, _, _, _, _) => {
	      implicit val req = request
	      ExactlyOne(
	        ExternalLinkType(routes.Application.createThing(context.request.ownerId.toThingId, context.state.toThingId, Some(thing.toThingId)).absoluteURL()))
	    }
	    case _ => QL.WarningValue("_createInstanceLink does not currently work outside of Play")
	  }
	})

  
  override lazy val props = Seq(
    classMethod,
    tooltipMethod,
    
    new SectionMethod,
    new LinkButtonMethod,
    new IconButtonMethod,
    new ShowLinkMethod,
    new PropLinkMethod,
    new CreateInstanceLinkMethod
  )
}