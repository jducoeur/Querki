package querki.html

import scala.xml.{Attribute, NodeSeq, Null, Text, Xhtml}

import play.api.templates.Html

import models.{DisplayText, HtmlWikitext, OID, QWikitext, SimplePTypeBuilder, UnknownOID, Wikitext}

import querki.core.URLableType
import querki.ecology._
import querki.ql.{QLPhrase, Signature, RequiredParam}
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
  val DataMethodOID = moid(3)
  val PageHeaderPropOID = moid(4)
  val QLButtonOID = moid(5)
}

/**
 * TODO: this should probably be merged with HtmlRenderer -- the distinction between them looks pretty
 * artificial from the outside.
 */
class UIModule(e:Ecology) extends QuerkiEcot(e) with HtmlUI with querki.core.MethodDefs {
  import UIMOIDs._

  lazy val HtmlRenderer = interface[querki.html.HtmlRenderer]
  lazy val Links = interface[querki.links.Links]
  lazy val QL = interface[querki.ql.QL]
  
  lazy val ExternalLinkType = Links.OldExternalLinkType
  lazy val ParsedTextType = QL.ParsedTextType

  /***********************************************
   * TYPES
   ***********************************************/

  /**
   * This is a fake PType, so that code can inject HTML into the pipeline.
   * 
   * Note that this doesn't get registered in System, since it doesn't exist from the User's perspective.
   */
  lazy val RawHtmlType = new SystemType[Wikitext](UnknownOID, () => models.Thing.emptyProps) with SimplePTypeBuilder[Wikitext]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = throw new Exception("Can't deserialize ParsedText!")
    def doSerialize(v:Wikitext)(implicit state:SpaceState) = throw new Exception("Can't serialize ParsedText!")
    def doWikify(context:QLContext)(v:Wikitext, displayOpt:Option[Wikitext] = None) = v
    
    def doDefault(implicit state:SpaceState) = Wikitext("")
  }

  def HtmlValue(html:Html):QValue = ExactlyOne(RawHtmlType(HtmlWikitext(html)))
  def HtmlValue(str:String):QValue = HtmlValue(Html(str))
  def HtmlValue(xml:NodeSeq):QValue = HtmlValue(Xhtml.toXhtml(xml))
  
  def toWikitext(xml:NodeSeq):Wikitext = HtmlWikitext(Html(Xhtml.toXhtml(xml)))

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
    def doTransform(nodes:NodeSeq, paramText:String, context:QLContext, params:Seq[QLPhrase]):NodeSeq
    
    override def qlApply(inv:Invocation):QValue = {
      val context = inv.context
      val paramsOpt = inv.paramsOpt
      
      val v = context.value
      if (v.pType != RawHtmlType && v.pType != ParsedTextType)
        throw new PublicException("UI.transform.htmlRequired", name)
      if (paramsOpt.isEmpty)
        throw new PublicException("UI.transform.classRequired", name)
      val params = paramsOpt.get

      def processHtml(content:DisplayText):HtmlWikitext = {
        val parsedParamOpt = context.parser.get.processPhrase(params(0).ops, context).value.firstTyped(ParsedTextType)
        if (parsedParamOpt.isEmpty) 
          throw new PublicException("UI.transform.classRequired", name)
        val paramText = parsedParamOpt.get.raw.toString
        val nodes = content.xml
        val newXml = nodes.flatMap(node => doTransform(node, paramText, context, params))
        val newHtml = Html(Xhtml.toXhtml(newXml))
        HtmlWikitext(newHtml)        
      }
      
      try {
	      v.pType match {
	        case RawHtmlType => {
		      v.map(RawHtmlType, RawHtmlType) { wikitext => processHtml(wikitext.display) }          
	        }
	        
	        case ParsedTextType => {
		      v.map(ParsedTextType, RawHtmlType) { wikitext => processHtml(wikitext.span) }          
	        }
	      }
      } catch {
        case ex:org.xml.sax.SAXParseException => {
          throw new PublicException("UI.transform.notWellFormed", name)
        }
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
    def doTransform(nodes:NodeSeq, paramText:String, context:QLContext, params:Seq[QLPhrase]):NodeSeq = HtmlRenderer.addClasses(nodes, paramText)
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
    def doTransform(nodes:NodeSeq, paramText:String, context:QLContext, params:Seq[QLPhrase]):NodeSeq = {
      val withClass = HtmlRenderer.addClasses(nodes, "_withTooltip")      
      XmlHelpers.mapElems(withClass)(_ % Attribute("title", Text(paramText), Null))
    }
  }
  
  lazy val dataMethod = new HtmlModifier(DataMethodOID, "_data",
      "Add HTML5 data to the received HTML value",
      """This is mainly for internal use for now. Similarly to the _class function, this lets
      |you add a data tag to a block. So for example, this:
      |[[_code(""[[""Hello world"" -> _data(""foo"", ""something"")]]"")]]
      |will add a "data-foo" attribute to the block containing Hello world.""".stripMargin)
  {
    def doTransform(nodes:NodeSeq, paramText:String, context:QLContext, params:Seq[QLPhrase]):NodeSeq = {
      if (params.length < 2)
        throw new PublicException("UI.transform.dataRequired")
      
      val dataBlock = context.parser.get.processPhrase(params(1).ops, context).value.firstTyped(ParsedTextType).
        getOrElse(throw new PublicException("UI.transform.dataRequired")).raw
      
      XmlHelpers.mapElems(nodes)(_ % Attribute(s"data-$paramText", Text(dataBlock), Null))
    }
  }
  
  lazy val PageHeaderProperty = new SystemProperty(PageHeaderPropOID, LargeTextType, Optional,
    toProps(
      setName("Page Header"),
      SkillLevel(SkillLevelAdvanced),
      Summary("Allows you to define the top of the page when looking at this Thing"),
      Details("""Normally, Querki displays each Thing with a fairly complex predefined header,
          |which includes its Display Name, Space, Model, edit buttons and so on. This works well
          |for most cases, but if you want more control over the look and feel of your display, you
          |can override that by setting this Property.""".stripMargin)))

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
    override def qlApply(inv:Invocation):QValue = {
      val context = inv.context
      val paramsOpt = inv.paramsOpt
    
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
        val processedHeader = parser.contextsToWikitext(Seq(parser.processPhrase(header.ops, context.forceAsCollection)))
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
      QL.WikitextValue(wikitext)
    }
  }

	abstract class ButtonBase(tid:OID, pf:PropFetcher) extends InternalMethod(tid, pf)
	{
	  def generateButton(url:String, params:Seq[Wikitext]):scala.xml.Elem
	  
	  def numParams:Int
	  
	  override def qlApply(inv:Invocation):QValue = {
	    val context = inv.context
	    val paramsOpt = inv.paramsOpt
	    
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
	  lazy val sig = Signature(ParsedTextType, RequiredParam("label"))
	  
	  override def qlApply(inv:Invocation):QValue = {
	    for (
	      pt <- inv.contextTypeAs[URLableType];
	      elemContext <- inv.contextElements;
	      elemV <- inv.opt(elemContext.value.firstOpt);
	      url <- inv.opt(pt.getURL(elemContext)(elemV));
	      paramVal <- inv.processParam(0, elemContext);
	      label = paramVal.wikify(elemContext);
	      wikitext = QWikitext("[") + label + QWikitext(s"]($url)")
	    )
	      yield QValue.make(ExactlyOne, ParsedTextType, wikitext)
	  }
	}
		
	class PropLinkMethod extends InternalMethod(PropLinkMethodOID, 
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
	          |This will work for any Property Type, even Types that don't really make sense as Views, so use with a bit
	          |of care!""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    for (
	      thing <- inv.contextAllThings;
	      prop <- inv.definingContextAsProperty
	    )
	      yield ExactlyOne(ExternalLinkType(thing.toThingId + "?prop=" + prop.toThingId))
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
	    case PlayRequestContext(request, _, _, _, _, _, _, _, _, _, _, _) => {
	      implicit val req = request
	      ExactlyOne(
	        ExternalLinkType(routes.Application.createThing(context.request.ownerId.toThingId, context.state.toThingId, Some(thing.toThingId)).absoluteURL()))
	    }
	    case _ => QL.WarningValue("_createInstanceLink does not currently work outside of Play")
	  }
	})

  lazy val QLButton = new InternalMethod(QLButtonOID,
    toProps(
      setName("_QLButton"),
      Summary("Shows a button that, when pressed, executes some QL and can show the result"),
      SkillLevel(SkillLevelAdvanced),
      Details("""    THING -> _QLButton(LABEL, QL, TARGET)
          |
          |This function is unusual, in that it is a way to do something only if the user presses a button.
          |It displays a button with the given LABEL; if the user presses that, it evaluates the given QL
          |(using the received THING as its context). 
          |
          |If a TARGET is specified, that should be the id of a div or span to put the results into; if it
          |is not given, the results will be displayed below the button.
          |
          |As an example of how to use this, say you have a complex Model Property that you want to make
          |editable on the Thing's page, but you only want to show it when needed. You can say:
          |
          |    \[[_QLButton(\""Edit My Model Property\"", My Model Property._edit)\]]
          |
          |In the long run, we will probably add better ways to handle user interaction. But this one is
          |relatively quick and easy for a few situations.""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QValue = {
	    for {
	      thing <- inv.contextFirstThing
	      labelWiki <- inv.processParamFirstAs(0, ParsedTextType)
	      label = HtmlEscape.escapeQuotes(labelWiki.raw.str.trim)
	      qlRaw <- inv.rawParam(1)
	      ql = HtmlEscape.escapeQuotes(qlRaw.reconstructString)
	      targetOptWiki = {
	        if (inv.numParams > 2)
	          inv.processParamFirstAs(2, ParsedTextType).get.headOption
	        else
	          None
	      }
	      targetName = targetOptWiki.map(_.raw.str.trim).getOrElse("target-" + scala.util.Random.nextInt.toString)
	      targetDiv = {
	        if (targetOptWiki.isEmpty)
	          s"""<div id="$targetName"></div>"""
	        else
	          ""
	      }
	    }
  	      yield HtmlValue(s"""<input type="button" value="$label" class="btn btn-primary _qlInvoke" data-thingid="${thing.toThingId}" data-target="$targetName" data-ql="$ql" href="#"></input>$targetDiv""")
	  }
	}
  
  override lazy val props = Seq(
    classMethod,
    tooltipMethod,
    dataMethod,
    PageHeaderProperty,
    
    new SectionMethod,
    new LinkButtonMethod,
    new IconButtonMethod,
    new ShowLinkMethod,
    new PropLinkMethod,
    new CreateInstanceLinkMethod,
    QLButton
  )
}