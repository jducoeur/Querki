package querki.html

import scala.xml.{Attribute, NodeSeq, Null, Text, Xhtml}

import scalatags.Text.all.{id => idAttr, i => iAttr, _}
import scalatags.Text.TypedTag

import org.jsoup
import jsoup.nodes.{Document => JDoc}
import jsoup.select.{Elements => JElems}

import models._

import querki.core.URLableType
import querki.ecology._
import querki.globals._
import querki.ql.{InvocationValue, QLExp, QLParam}
import querki.util.{HtmlEscape, SafeUrl, XmlHelpers}
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
  val MixedButtonOID = moid(6)
  val CreateButtonOID = moid(7)
  val ShowSomeOID = moid(8)
  val QLLinkOID = moid(9)
  val QLTreeOID = moid(10)
  val MenuButtonOID = moid(11)
  val QLInputOID = moid(12)
  val UniqueHtmlIdOID = moid(13)
  val UpdateableSectionOID = moid(14)
  val UpdateSectionOID = moid(15)
}

/**
 * TODO: this should probably be merged with HtmlRenderer -- the distinction between them looks pretty
 * artificial from the outside.
 */
class UIModule(e:Ecology) extends QuerkiEcot(e) with HtmlUI with querki.core.MethodDefs {
  import UIMOIDs._

  val Basic = initRequires[querki.basic.Basic]
  lazy val HtmlRenderer = interface[querki.html.HtmlRenderer]
  lazy val Links = interface[querki.links.Links]
  val Logic = initRequires[querki.logic.Logic]
  lazy val PublicUrls = interface[PublicUrls]
  val QL = initRequires[querki.ql.QL]
  lazy val Tags = interface[querki.tags.Tags]
  
  lazy val ExternalLinkType = Links.URLType
  lazy val NewTagSetType = Tags.NewTagSetType
  lazy val ParsedTextType = QL.ParsedTextType

  /***********************************************
   * TYPES
   ***********************************************/

  /**
   * This is a fake PType, so that code can inject HTML into the pipeline.
   * 
   * The odd name is because this is effectively an error in some cases: it shows up when, for instance,
   * you have a Required Thing that points to -1.
   */
  lazy val RawHtmlType = new SystemType[Wikitext](UnknownOID, 
    toProps(
      setName("Value has not been set"),
      setInternal,
      Basic.DisplayTextProp("""This shows up when you have a Required Property (usually a Required Thing)
        |or a List Element
        |that hasn't actually been set to a value. Set the Property to something real in order to make this
        |message go away.
        |
        |(If you never want to see this message, set the Property to a default value in its Model.)
        """.stripMargin))) with SimplePTypeBuilder[Wikitext]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = throw new Exception("Can't deserialize ParsedText!")
    def doSerialize(v:Wikitext)(implicit state:SpaceState) = throw new Exception("Can't serialize ParsedText!")
    def doWikify(context:QLContext)(v:Wikitext, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = 
      Future.successful(v)
    
    def doDefault(implicit state:SpaceState) = Wikitext("")
    
    // Transient values, so we don't care:
    def doComputeMemSize(v:Wikitext):Int = 0
  }

  def HtmlValue(html:QHtml):QValue = ExactlyOne(RawHtmlType(HtmlWikitext(html)))
  def HtmlValue(str:String):QValue = HtmlValue(QHtml(str))
  def HtmlValue(xml:NodeSeq):QValue = HtmlValue(Xhtml.toXhtml(xml))
  def HtmlValue(tag:TypedTag[_]):QValue = ExactlyOne(RawHtmlType(HtmlWikitext(tag.toString)))
  
  def toWikitext(xml:NodeSeq):Wikitext = HtmlWikitext(QHtml(Xhtml.toXhtml(xml)))
  
  override lazy val types = Seq(
    RawHtmlType
  )

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
      Categories(UITag),
      Summary(summary),
      Details(details))) 
  {
    // Actual Modifier classes should implement this, which does the heart of the work
    def doTransform(elems:JElems, paramText:String, context:QLContext, params:Seq[QLParam]):Future[JElems]
    
    override def qlApply(inv:Invocation):QFut = {
      val context = inv.context
      val paramsOpt = inv.paramsOpt
      
      if (paramsOpt.isEmpty)
        throw new PublicException("UI.transform.classRequired", name)
      val params = paramsOpt.get
      val v = context.value
      if (v.isEmpty) {
        // We allow empty values to pass quietly through:
        fut(EmptyValue(QL.ParsedTextType))
      } else {
        def contentToUse:InvocationValue[DisplayText] = {
          v.pType match {
            case RawHtmlType => 
              for {
                wikitext <- inv.contextAllAs(RawHtmlType)
              }
                yield wikitext.display
            case ParsedTextType =>
              for {
                wikitext <- inv.contextAllAs(ParsedTextType)
              }
                yield wikitext.span
            case _ => {
              val wikitext = context.value.wikify(context).map(_.raw)
              inv.fut(wikitext)
            }
          }
        }
        
        for {
          parsedParam <- inv.processParamFirstAs(0, ParsedTextType)
          paramText = parsedParam.raw.toString
          content <- contentToUse          
          doc = jsoup.Jsoup.parse(content.str)
                  .outputSettings((new jsoup.nodes.Document.OutputSettings).escapeMode(jsoup.nodes.Entities.EscapeMode.xhtml))
          realDoc =
            if (doc.body.children.isEmpty)
              // There are no child nodes, which implies that this is just top-level text. In that case, wrap
              // it in a span, so we have something to manipulate:
              jsoup.Jsoup.parse(s"<span>${content.str}</span>")
                  .outputSettings((new jsoup.nodes.Document.OutputSettings).escapeMode(jsoup.nodes.Entities.EscapeMode.xhtml))
            else
              doc
          elems <- inv.fut(doTransform(realDoc.body.children, paramText, context, params))
          newHtml = QHtml(elems.toString)
        }
          yield QL.WikitextValue(HtmlWikitext(newHtml))
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
            |paragraph. (This is less often necessary, but occasionally helpful.)
            |
            |If _class receives a value other than text or HTML, it will render that value, and then apply the
            |class to it. Therefore, this should usually be at the *end* of your phrase.""".stripMargin)
  {
    def doTransform(elems:JElems, paramText:String, context:QLContext, params:Seq[QLParam]):Future[JElems] = 
      fut(elems.addClass(paramText))
//      Future.successful(HtmlRenderer.addClasses(nodes, paramText))
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
    def doTransform(elems:JElems, paramText:String, context:QLContext, params:Seq[QLParam]):Future[JElems] = {
      val withClass = elems.addClass("_withTooltip")
      fut(elems.attr("title", paramText))
//      val withClass = HtmlRenderer.addClasses(nodes, "_withTooltip")      
//      Future.successful(XmlHelpers.mapElems(withClass)(_ % Attribute("title", Text(paramText), Null)))
    }
  }
  
  lazy val dataMethod = new HtmlModifier(DataMethodOID, "_data",
      "Add HTML5 data to the received HTML value",
      """This is mainly for internal use for now. Similarly to the _class function, this lets
      |you add a data tag to a block. So for example, this:
      |[[_code(""[[""Hello world"" -> _data(""foo"", ""something"")]]"")]]
      |will add a "data-foo" attribute to the block containing Hello world.""".stripMargin)
  {
    def doTransform(elems:JElems, paramText:String, context:QLContext, params:Seq[QLParam]):Future[JElems] = {
      if (params.length < 2)
        throw new PublicException("UI.transform.dataRequired")
      
      for {
        processed <- context.parser.get.processExp(params(1).exp, context)
        dataBlock = processed.value.firstTyped(ParsedTextType).getOrElse(throw new PublicException("UI.transform.dataRequired")).raw
      }
        yield elems.attr(s"data-$paramText", dataBlock) 
        //XmlHelpers.mapElems(nodes)(_ % Attribute(s"data-$paramText", Text(dataBlock), Null))
    }
  }
  
  lazy val PageHeaderProperty = new SystemProperty(PageHeaderPropOID, LargeTextType, Optional,
    toProps(
      setName("Page Header"),
      SkillLevel(SkillLevelAdvanced),
      Categories(UITag),
      Summary("Allows you to define the top of the page when looking at this Thing"),
      Details("""Normally, Querki displays each Thing with a fairly complex predefined header,
          |which includes its Name, Space, Model, edit buttons and so on. This works well
          |for most cases, but if you want more control over the look and feel of your display, you
          |can override that by setting this Property.""".stripMargin)))

  /***********************************************
   * FUNCTIONS
   ***********************************************/

  class SectionMethod extends InternalMethod(SectionMethodOID,
    toProps(
      setName("_section"),
      Categories(UITag),
      Summary("Display a List as a Header, followed by its contents"),
      Details("""_section is intended for the common case where you want to display a section
          |on the page if and only if a specific List is non-empty. It looks like this:
          |    My List -> _section(HEADER, DETAILS, EMPTY)
          |Each of the parameters can be any QL phrase, although they are often just text blocks. They are
          |treated as follows:
          |
          |* HEADER is shown first, if the incoming List is non-empty. It gets the entire List as its Context.
          |* DETAILS is shown after the header, also getting the incoming List as its Context.
          |* EMPTY is shown if and only if the List is empty. This lets you show something else if appropriate.
          |It is optional -- you can leave it off.
          |
          |Note that the generated QText will have the HEADER on a separate line from the DETAILS. This is
          |usually what you want. It means that, for example, if you start the HEADER with "###", it will show
          |up as a true header, separate from the DETAILS, but if it is just something like "Contents:", the
          |HEADER and DETAILS will run together, since QText joins ordinary text lines together.""".stripMargin)
    )) 
  {
    override def qlApply(inv:Invocation):QFut = {
      val context = inv.context
      val paramsOpt = inv.paramsOpt
    
      paramsOpt match {
        case Some(params) if (params.length > 0) => {
          val header = params(0).exp
          val details = if (params.length > 1) Some(params(1).exp) else None
          val empty = if (params.length > 2) Some(params(2).exp) else None
          buildSection(context, header, details, empty)
        }
        case _ => Future.successful(QL.ErrorValue("_section requires at least one parameter"))
      }
    }

    def buildSection(context:QLContext, header:QLExp, detailsOpt:Option[QLExp], emptyOpt:Option[QLExp]):QFut = {
      val parser = context.parser.get
      val wikitextFut = if (context.isEmpty) {
        emptyOpt match {
          case Some(empty) => parser.contextsToWikitext(parser.processExpAll(empty, context.root))
          case _ => fut(Wikitext(""))
        }
      } else {
        for {
          processedHeader <- parser.contextsToWikitext(parser.processExpAll(header, context.forceAsCollection))
          processedDetails = detailsOpt.map(details => parser.processExpAll(details, context))
          detailContents <- processedDetails.map(parser.contextsToWikitext(_, true)).getOrElse(Future.successful(Wikitext("")))
        }
          yield processedHeader + detailContents
      }
      wikitextFut.map(wikitext => QL.WikitextValue(wikitext))
    }
  }

  abstract class ButtonBase(tid:OID, pf:PropMap) extends InternalMethod(tid, pf)
  {
    def generateButton(url:String, params:Seq[Wikitext]):scala.xml.Elem
    
    def numParams:Int
    
    override def qlApply(inv:Invocation):QFut = {
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
              Future.sequence(
                params.map(param => 
                  context.parser.get.processExp(param.exp, context).flatMap(_.value.wikify(context))))
              .map { paramTexts =>
                HtmlValue(QHtml(generateButton(url, paramTexts).toString))
              }
            }
            // Nothing incoming, so cut.
            // TODO: there is probably a general pattern to pull out here, of "cut processing if the input is empty"
            case None => Future.successful(EmptyValue(RawHtmlType))
          }
        }
        case None => QL.WarningFut(displayName + " requires " + numParams + " parameters.")
      }
    }
  }

  /**
   * TODO: add parameters to specify the common style decisions: size (btn-sm, btn-xs, etc) and color (btn-default,
   * btn-warning, btn-danger, etc). These should each take a well-defined enumeration.
   */
  class LinkButtonBase(oid: OID, name: String, kind: String, isButton: Boolean) extends InternalMethod(oid,
    toProps(
      setName(name),
      Categories(UITag),
      Summary(s"Displays a $kind that goes to a linked page when you press it."),
      Signature(
        expected = Some((Seq(LinkType, ExternalLinkType), s"The Thing or page to go to when this $kind is pressed")),
        reqs = Seq.empty,
        opts = Seq(
          ("label", ParsedTextType, Core.QNone, s"The text to display on this $kind, if any"),
          ("icon", ParsedTextType, Core.QNone, s"The icon to display on this $kind, if any"),
          ("tooltip", ParsedTextType, Core.QNone, s"The tooltip to show when the user hovers over this $kind"),
          ("id", ParsedTextType, Core.QNone, s"The HTML id to give to this $kind")
        ),
        returns = (RawHtmlType, s"The $kind as requested")
      ),
      Details(s"""$name receives a Link or External Link, and displays that
          |link as a $kind. You should always provide at least a label or an icon.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        pt <- inv.contextTypeAs[URLableType]
        elemContext <- inv.contextElements
        url <- inv.opt(pt.getURL(elemContext)(elemContext.value.first))
        labelOpt <- inv.processAsOpt("label", ParsedTextType, elemContext)
        iconOpt <- inv.processAsOpt("icon", ParsedTextType, elemContext)
        tooltipOpt <- inv.processAsOpt("tooltip", ParsedTextType, elemContext)
        idOpt <- inv.processAsOpt("id", ParsedTextType, elemContext)
        openNewWindow = 
          (url.startsWith("http:") ||
           url.startsWith("https:") ||
           url.startsWith("mailto:"))
      }
        yield {
          HtmlValue(
            a(
              if (isButton)
                cls:="btn btn-primary",
              href:=url,
              idOpt.map { idStr => idAttr:=idStr.raw.toString },
              tooltipOpt.map { tooltip => title:=tooltip.raw.toString },
              iconOpt.map { icon => iAttr(cls:=s"glyphicon glyphicon-${icon.raw.toString}") },
              if (openNewWindow)
                target := "_blank",
              if (iconOpt.isDefined && labelOpt.isDefined)
                // Need a space between them:
                " ",
              labelOpt.map { label => label.displayWith(new LiteralTransformWrapper(false)).toString }
            )
          )
        }
    }
  }

  /**
   * TODO: add parameters to specify the common style decisions: size (btn-sm, btn-xs, etc) and color (btn-default,
   * btn-warning, btn-danger, etc). These should each take a well-defined enumeration.
   */
  class LinkButtonMethod extends LinkButtonBase(LinkButtonOID, "_linkButton", "button", true)
  class ShowLinkMethod extends LinkButtonBase(ShowLinkMethodOID, "_showLink", "link", false)
  
  class IconButtonMethod extends ButtonBase(IconButtonOID,
    toProps(
      setName("_iconButton"),
      Basic.DeprecatedProp(true),
      Summary("Displays a button showing an icon, that goes to a linked page when you press it."),
      Details("""    LINK -> _iconButton(ICON, TOOLTIP)
          |**DEPRECATED:** use _linkButton instead.
          |
          |_iconButton receives a Link or External Link, and displays that
          |link as a button. The first parameter identifies the icon to use for the button; the second is the
          |hover text to display as a tooltip. Both parameters are required.
          |
          |For icons, you may use anything from the [Bootstrap Glyphicon](http://getbootstrap.com/2.3.2/base-css.html#icons) set.
          |Just use the name of the icon (in double-double quotes) in the parameter.""".stripMargin)))
  {
    val numParams = 2
    
    def generateButton(url:String, params:Seq[Wikitext]):scala.xml.Elem = {
      <a class="btn btn-default btn-xs btn-default" href={url} title={params(1).raw}><i class={"glyphicon glyphicon-" + params(0).raw}></i></a>
    }
  }
  
  class MixedButtonMethod extends ButtonBase(MixedButtonOID,
    toProps(
      setName("_mixedButton"),
      Basic.DeprecatedProp(true),
      Summary("Displays a button showing an icon and a text label, that goes to a linked page when you press it."),
      Details("""    LINK -> _mixedButton(ICON, LABEL)
          |**DEPRECATED:** use _linkButton instead.
          |
          |_mixedButton receives a Link or External Link, and displays that
          |link as a button. The first parameter identifies the icon to use for the button; the second is the
          |text that follows the icon. Both parameters are required. This is essentially a combo of _iconButton
          |and _linkButton.
          |
          |For icons, you may use anything from the [Bootstrap Glyphicon](http://getbootstrap.com/2.3.2/base-css.html#icons) set.
          |Just use the name of the icon (in double-double quotes) in the parameter.""".stripMargin)))
  {
    val numParams = 2
    
    def generateButton(url:String, params:Seq[Wikitext]):scala.xml.Elem = {
      <a class="btn btn-default btn-sm btn-primary" href={url}><i class={"glyphicon glyphicon-" + params(0).raw}></i> {params(1).raw}</a>
    }
  }
  
//  // TODO: this is very similar to _linkButton, and should be refactored.
//  class ShowLinkMethod extends InternalMethod(ShowLinkMethodOID,
//    toProps(
//      setName("_showLink"),
//      Categories(UITag),
//      Summary("Displays a Link or External Link as a normal HTML link."),
//      Details("""    LINK -> _showLink(LABEL)
//          |This is the most normal way to display a Link or External Link with a chosen label. The
//          |label may be any expression you choose.
//          |
//          |The default behaviour of a Link, if you don't do anything with it, is effectively
//          |"_showLink(Default View)".""".stripMargin)))
//  {
//    override def qlApply(inv:Invocation):QFut = {
//      for {
//        pt <- inv.contextTypeAs[URLableType]
//        elemContext <- inv.contextElements
//        elemV <- inv.opt(elemContext.value.firstOpt)
//        url <- inv.opt(pt.getURL(elemContext)(elemV))
//        paramVal <- inv.processParam(0, elemContext)
//        label <- inv.fut(paramVal.wikify(elemContext))
//        wikitext = QWikitext("[") + label + QWikitext(s"]($url)")
//      }
//        yield QValue.make(ExactlyOne, ParsedTextType, wikitext)
//    }
//  }
    
  class PropLinkMethod extends InternalMethod(PropLinkMethodOID, 
    toProps(
      setName("_propLink"),
      Categories(UITag),
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
    override def qlApply(inv:Invocation):QFut = {
      for (
        thing <- inv.contextAllThings;
        prop <- inv.definingContextAsProperty
      )
        yield ExactlyOne(ExternalLinkType(thing.toThingId + "?prop=" + prop.toThingId))
    }
  }
  
  def getCreateInstanceUrl(inv:Invocation):InvocationValue[String] = {
    implicit val state = inv.state
      // First, figure out the linkback if there is one:
      val linkParamFut:Future[String] = inv.definingContext match {
        case Some(definingContext) => {
          val invStr = for {
            lexicalThing <- inv.opt(inv.lexicalThing match { case Some(t:Thing) => Some(t); case _ => None})
            linkProp <- inv.definingContextAsProperty
            fieldId = new FieldIds(None, linkProp)
            backLink <- inv.opt(linkProp.pType match {
              case pt:querki.core.IsLinkType => Some(lexicalThing.id.toThingId.toString)
              case NewTagSetType => lexicalThing.linkName
              case _ => None
            })
            backLinkSafe = SafeUrl(backLink)
          }
            yield s"&${fieldId.inputControlId}=$backLinkSafe"
            
          invStr.get.map(_.headOption.getOrElse(""))
        }
        case _ => Future.successful("")
      }
      
      for {
        thing <- inv.contextFirstThing
        url <- inv.fut(linkParamFut.map(PublicUrls.createAndEditUrl(inv.context.request, thing.toThingId) + _))
      }
        yield url
  }
  
  lazy val CreateInstanceLinkMethod = new InternalMethod(CreateInstanceLinkOID,
    toProps(
      setName("_createInstanceLink"),
      Categories(UITag),
      Summary("Given a received Model, this produces a Link to create an instance of that Model."),
      Details("""```
        |MODEL -> _createInstanceLink -> _linkButton(LABEL)
        |```
        |This is how you implement a "Create" button. _createInstanceLink takes a MODEL, and produces an External Link to the page to create a new Instance of it.
        |
        |You will usually then feed this into, eg, _linkButton or _iconButton as a way to display the Link.
        |```
        |MODEL -> LINK PROPERTY._createInstanceLink -> _linkButton(LABEL)
        |```
        |
        |You may optionally specify a Link Property with _createInstanceLink. That means that the newly-created Instance
        |should point back to *this* Thing -- the one where you have the button -- through the specified Link Property.
        |This is very useful for creating "child" Thing easily.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        url <- getCreateInstanceUrl(inv)
      }
        yield ExactlyOne(ExternalLinkType(url))
    }
  }
  
  lazy val CreateButtonFunction = new InternalMethod(CreateButtonOID,
    toProps(
      setName("_createButton"),
      Categories(UITag),
      Summary("Becomes a Create button for the received Model"),
      Signature(
        expected = Some((Seq(LinkType), "The Model to instantiate")),
        reqs = Seq(("label", TextType, "The text to show on the button")),
        opts = Seq(("classes", TextType, Core.QNone, "Display classes to apply to the button. If not specified, btn-default will be used.")),
        returns = (RawHtmlType, "The actual button on the page"),
        defining = Some(false, Seq(LinkType), "A Property -- if given, that Property on the new Instance will point back to here")
      ),
      Details("""This displays a button, with the given **label**, if the user is allowed to create Instances of that Model.
          |
          |For example, say we have Models named Bookcase and Book. Book has a My Bookcase property, that points to the case it
          |is on. I want a button to show on Bookcase that creates a new Book on that case, and I want it to show using the "primary"
          |style (green). I would add a button to Bookcase, that looks like this:
          |```
          |\[[Book -> My Bookcase._createButton(\""Add a Book to this case\"", classes=\""btn-primary\"")\]]
          |```
          |Because I give My Bookcase as the Defining Context, Querki uses that property to point from the new Book back
          |to the Bookcase that created it.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        url <- getCreateInstanceUrl(inv)
        labelWikitext <- inv.processAs("label", ParsedTextType)
        label = labelWikitext.raw.str
        classesWikiOpt <- inv.processAsOpt("classes", ParsedTextType)
        allClasses =
          "btn" +
          classesWikiOpt.map(_.raw.str).map(" " + _).getOrElse(" btn-default")
      }
        yield 
          HtmlValue(
            a(
              cls := allClasses,
              href := url,
              label
            )
          )
    }
  }
  
  abstract class ClickableQLBase(oid:OID, pf:PropMap) extends InternalMethod(oid, pf)
  {
    def buildHtml(label:String, core:String):String
    
    override def qlApply(inv:Invocation):QFut = {
      val qv = inv.context.value
      // TODO: for the moment, we're using a single context for the label (see below). If that's
      // empty, we synthesize one. This fixed QI.9v5kari, but kind of sucks. Can we do better?
      val elem = qv.elems.headOption.getOrElse(Logic.True)
      val singleContext = inv.context.next(ExactlyOne(elem))
      for {
        qlRaw <- inv.rawRequiredParam("ql")
        ql = HtmlEscape.escapeQuotes(qlRaw.reconstructStandalone)
        pt = qv.pType
        targetOpt <- inv.processAsOpt("target", ParsedTextType)
        (targetName, targetDiv) =
          targetOpt match {
            case Some(target) => (target.raw.str.trim, "")
            case None => {
              val name = "target-" + scala.util.Random.nextInt.toString 
              (name, s"""<div id="$name"></div>""")
            }
          }
        append <- inv.processAs("append", YesNoType)
        replace <- inv.processAs("replace", YesNoType)
        noIcon <- inv.processAs("noIcon", YesNoType)
        noDiv <- inv.processAs("noDiv", YesNoType)
        updateSectionAfter <- inv.processAs("updateSectionAfter", YesNoType)
        // QI.9v5kage: the singleContext here fixes the bug, resulting in only a single button.
        // TODO: why does this require singleContext? If this *specific* parameter uses the full
        // context, we can wind up with multiple buttons if the context contains multiple elements.
        // What is it about ParsedTextType that is causing this, the auto-deconstruction of ""...""? We'd
        // actually like this to *process* with the full context, but only *result* in a single
        // value afterwards.
        labelWiki <- inv.processAs("label", ParsedTextType, singleContext)
        label = HtmlEscape.escapeQuotes(labelWiki.raw.str.trim)
        lexicalBundleOpt = inv.context.parser.flatMap(_.lexicalThing)
        lexicalThingOpt = lexicalBundleOpt.flatMap(_ match { case t:Thing => Some(t); case _ => None }) 
        serialized <- QL.serializeContext(inv, Some("ql"))
      }
        yield 
          HtmlValue(
            buildHtml(
              label, 
              s"""data-ptype="${pt.id.toThingId}" data-context=".$serialized" data-target="$targetName" data-ql="$ql" data-append="$append" """ +
              s"""data-replace="$replace" data-noicon="$noIcon" data-updatesection="$updateSectionAfter" """ +
              (if (noDiv) """data-nodiv="true" """ else "") +
              s"""${lexicalThingOpt.map(lex => s"""data-lexical="${lex.id.toThingId}"""").getOrElse("")} href="#" """) 
            + targetDiv)
    }
  }

  lazy val QLButton = new ClickableQLBase(QLButtonOID,
    toProps(
      setName("_QLButton"),
      Categories(UITag),
      Signature(
        expected = Some((Seq(AnyType), "The value that will be passed to the QL.")),
        reqs = Seq(
          ("label", ParsedTextType, "The text to display on the button."),
          ("ql", Basic.QLType, "The QL to execute when the button is pressed.")
        ),
        opts = Seq(
          ("target", ParsedTextType, Core.QNone, """The id of a div or span to put the results into; if it
                |is not given, the results will be displayed below the button.""".stripMargin),
          ("append", YesNoType, ExactlyOne(YesNoType(false)), """If set to true, pressing the button again runs
                |the QL again, and appends the result to the target div. Otherwise, pressing the button again
                |closes the div.""".stripMargin),
          ("replace", YesNoType, ExactlyOne(YesNoType(false)), """If set to true, pressing the button again re-runs
                |the QL, and replaces the value in the target div. (This is rarely useful.)""".stripMargin),
          ("noIcon", YesNoType, ExactlyOne(YesNoType(false)), """Normally, Querki adds an indicator icon to the button,
                |showing that it opens and closes. If `noIcon` is set to `True`, that won't be shown.""".stripMargin),
          ("noDiv", YesNoType, ExactlyOne(YesNoType(false)), """Normally, Querki displays the result of the QL expression
                |in a div. If `noDiv` is set to `True`, the results will be added with nothing around them.""".stripMargin),
          ("updateSectionAfter", YesNoType, ExactlyOne(YesNoType(false)), """If this is contained within an `_updateableSection`,
                |and you set `updateSectionAfter` to `true`, that enclosing section will be redisplayed after this is clicked.""".stripMargin)
        ),
        returns = (RawHtmlType, "The button")
      ),
      Summary("Shows a button that, when pressed, executes some QL and can show the result"),
      SkillLevel(SkillLevelAdvanced),
      Details("""This function is unusual, in that it is a way to do something only if the user presses a button.
          |It displays a button with the given **label**; if the user presses that, it evaluates the given **ql**
          |(using the received Thing as its context).
          |
          |As an example of how to use this, say you have a complex Model Property that you want to make
          |editable on the Thing's page, but you only want to show it when needed. You can say:
          |
          |    \[[_QLButton(\""Edit My Model Property\"", My Model Property._edit)\]]""".stripMargin)))
  {
    def buildHtml(label:String, core:String):String = {
      s"""<a class="btn btn-primary _qlInvoke" $core>$label</a>"""
    }
  }

  lazy val QLLink = new ClickableQLBase(QLLinkOID,
    toProps(
      setName("_QLLink"),
      Signature(
        expected = Some((Seq(AnyType), "The value that will be passed to the QL.")),
        reqs = Seq(
          ("label", ParsedTextType, "The text to display on the button."),
          ("ql", Basic.QLType, "The QL to execute when the button is pressed.")
        ),
        opts = Seq(
          ("target", ParsedTextType, Core.QNone, """The id of a div or span to put the results into; if it
                |is not given, the results will be displayed below the button.""".stripMargin),
          ("append", YesNoType, ExactlyOne(YesNoType(false)), """If set to true, clicking the link again runs
                |the QL again, and appends the result to the target div. Otherwise, clicking the link again
                |closes the div.""".stripMargin),
          ("replace", YesNoType, ExactlyOne(YesNoType(false)), """If set to true, clicking the link again re-runs
                |the QL, and replaces the value in the target div. (This is rarely useful.)""".stripMargin),
          ("noIcon", YesNoType, ExactlyOne(YesNoType(false)), """Normally, Querki adds an indicator icon to the link,
                |showing that it opens and closes. If `noIcon` is set to `True`, that won't be shown.""".stripMargin),
          ("noDiv", YesNoType, ExactlyOne(YesNoType(false)), """Normally, Querki displays the result of the QL expression
                |in a div. If `noDiv` is set to `True`, the results will be added with nothing around them.""".stripMargin),
          ("updateSectionAfter", YesNoType, ExactlyOne(YesNoType(false)), """If this is contained within an `_updateableSection`,
                |and you set `updateSectionAfter` to `true`, that enclosing section will be redisplayed after this is clicked.""".stripMargin)
        ),
        returns = (RawHtmlType, "The link, ready for the page")
      ),
      Categories(UITag),
      Summary("Shows a link that, when clicked, executes some QL and can show the result"),
      SkillLevel(SkillLevelAdvanced),
      Details("""This function is unusual, in that it is a way to do something only if the user clicks a link.
          |It displays a link with the given **label**; if the user clicks that, it evaluates the given **ql**
          |(using the received Thing as its context).
          |
          |As an example of how to use this, say you have a complex Model Property that you want to make
          |editable on the Thing's page, but you only want to show it when needed. You can say:
          |
          |    \[[_QLLink(\""Edit My Model Property\"", My Model Property._edit)\]]""".stripMargin)))
  {
    def buildHtml(label:String, core:String):String = {
      s"""<a class="_qlInvoke" $core>$label</a>"""
    }
  }

  lazy val QLInput = new ClickableQLBase(QLInputOID,
    toProps(
      setName("_QLInput"),
      Signature(
        expected = Some((Seq(AnyType), "The value that will be passed to the QL.")),
        reqs = Seq(
          ("label", ParsedTextType, "The text to display, greyed-out, in the input, to prompt the user."),
          ("ql", Basic.QLType, "The QL to execute when the user presses Enter.")
        ),
        opts = Seq(
          ("target", ParsedTextType, Core.QNone, """The id of a div or span to put the results into; if it
                |is not given, the results will be displayed below the input.""".stripMargin),
          ("append", YesNoType, ExactlyOne(YesNoType(false)), """If set to true, entering something else runs
                |the QL again, and appends the result to the target div. Otherwise, pressing Enter again
                |closes the div.""".stripMargin),
          ("replace", YesNoType, ExactlyOne(YesNoType(false)), """If set to true, pressing Enter again re-runs
                |the QL with the new value of the input field, and replaces the previous result in the target div.""".stripMargin),
          ("noIcon", YesNoType, ExactlyOne(YesNoType(false)), """Not currently used in _QLInput.""".stripMargin),
          ("noDiv", YesNoType, ExactlyOne(YesNoType(false)), """Normally, Querki displays the result of the QL expression
                |in a div. If `noDiv` is set to `True`, the results will be added with nothing around them.""".stripMargin),
          ("updateSectionAfter", YesNoType, ExactlyOne(YesNoType(false)), """If this is contained within an `_updateableSection`,
                |and you set `updateSectionAfter` to `true`, that enclosing section will be redisplayed after this is clicked.""".stripMargin)
        ),
        returns = (RawHtmlType, "The input field, ready for the page")
      ),
      Categories(UITag),
      Summary("Shows an input field that, when the user types some text and presses Enter, executes some QL and can show the result"),
      SkillLevel(SkillLevelAdvanced),
      Details("""This function is unusual, in that it is a way to do something only if the user enters some text.
          |It displays an input with the given **placeholder**; if the user clicks that, it evaluates the given **ql**
          |(using the received Thing as its context).
          |
          |The entered input text is available in the QL expression as $input.""".stripMargin)))
  {
    def buildHtml(label:String, core:String):String = {
      s"""<input type="text" class="_qlInvoke" placeholder="$label" $core></input>"""
    }
  }
  
  lazy val UpdateableSection = new InternalMethod(UpdateableSectionOID,
    toProps(
      setName("_updateableSection"),
      SkillLevel(SkillLevelAdvanced),
      Categories(UITag),
      Signature(
        expected = None,
        reqs = Seq(
          ("contents", AnyType, "The contents of this section")
        ),
        opts = Seq.empty,
        returns = (RawHtmlType, "The rendered section, which can be updated by calling `_updateSection` from inside.")
      )))
  {
    override def qlApply(inv: Invocation): QFut = {
      val qv = inv.context.value
      for {
        // The contents in serializable form:
        contentsRaw <- inv.rawRequiredParam("contents")
        contentsQuoted = HtmlEscape.escapeQuotes(contentsRaw.reconstructStandalone)
        // And the contents actually rendered, to display:
        contents <- inv.process("contents")
        pt = qv.pType
        lexicalBundleOpt = inv.context.parser.flatMap(_.lexicalThing)
        lexicalThingOpt = lexicalBundleOpt.flatMap(_ match { case t:Thing => Some(t); case _ => None }) 
        contentsWiki <- inv.fut(contents.wikify(inv.context, None, lexicalThingOpt))
        serialized <- QL.serializeContext(inv, Some("contents"))
        targetName = "target-" + scala.util.Random.nextInt.toString
      }
        yield 
          QL.WikitextValue(
            HtmlWikitext(
              s"""<updateable id="$targetName" data-ptype="${pt.id.toThingId}" data-context=".$serialized" data-ql="$contentsQuoted"""" +
              s"""${lexicalThingOpt.map(lex => s"""data-lexical="${lex.id.toThingId}"""").getOrElse("")} href="#" """ +
              ">"
            ) +
            contentsWiki +
            HtmlWikitext(s"""</updateable>""")
          )
    }
  }
  
  lazy val ThingTree = new InternalMethod(QLTreeOID,
    toProps(
      setName("_thingTree"),
      SkillLevel(SkillLevelAdvanced),
      Categories(UITag),
      Signature(
        expected = Some((Seq(LinkType), "One or more Things to display as equal nodes in a tree")),
        reqs = Seq(),
        opts = Seq(
          ("text", TextType, Core.QNone, "The text to display for this node. If omitted, will be shown as a link to this Thing as usual."),
          ("children", Basic.QLType, Core.QNone, "A QL expression that produces the children of this node, which should be more _QLTrees. If empty, this node is a leaf."),
          ("id", TextType, Core.QNone, "The jsTree id to use for this node."),
          ("icon", TextType, Core.QNone, "The icon to display for this node. If not specified, no icon will be shown."),
          ("opened", YesNoType, ExactlyOne(Logic.False), "If set to True, children of this node will be displayed immediately.")
        ),
        returns = (RawHtmlType, "The tree, or a child node under a higher-level tree")
      ),
      Summary("Display a tree node, which will invoke the specified QL code to get its children")))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        (bundle, elem) <- inv.contextBundlesAndContexts
        thing <- inv.opt(bundle.asThing)
        textOpt <- inv.processAsOpt("text", ParsedTextType, elem)
        text <- inv.fut(textOpt.map(Future.successful(_)).getOrElse(elem.value.wikify(elem)))
        phraseOpt <- inv.rawParam("children")
        qlOpt = phraseOpt.map(phrase => HtmlEscape.escapeQuotes(phrase.reconstructStandalone))
        iconOpt <- inv.processAsOpt("icon", ParsedTextType, elem)
        idOpt <- inv.processAsOpt("id", ParsedTextType, elem)
        opened <- inv.processAs("opened", YesNoType, elem)
      }
        yield 
          HtmlValue(
            span(
              cls:="_qlTree",
              iconOpt.map(icon => data.icon := icon.raw.toString),
              data.opened := opened,
              data.thingid := thing.toThingId.toString,
              idOpt.map(id => data.id := id.raw.toString),
              raw(text.raw),
              qlOpt.map(ql => span(cls:="_treeQL", raw(ql), display:="none"))
            )
          )
    }
  }
  
  lazy val MenuButton = new InternalMethod(MenuButtonOID,
    toProps(
      setName("_menuButton"),
      SkillLevel(SkillLevelAdvanced),
      Categories(UITag),
      Summary("Use this to define a button that, when pressed, will mimic the effect of the specified menu item"),
      Signature(
        expected = None,
        reqs = Seq(
          ("id", TextType, "The HTML id of the menu item to invoke"),
          ("label", TextType, "What to show on the button")
        ),
        opts = Seq(
          ("class", TextType, Core.QNone, "Additional space-separated Bootstrap classes to use for displaying this button")
        ),
        returns = (RawHtmlType, "The button")
      ),
      Details("""Sometimes, you want to be able to create a button that mimics the effect of a menu item. This
        |function allows you to do that.
        |
        |For example, the "Design a New Model" button on the default Space page looks like this:
        |```
        |_menuButton(\""designAModel\"", \""Design a New Model\"", class=\""btn-xs btn-primary\"")
        |```
        |""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        id <- inv.processAs("id", ParsedTextType)
        label <- inv.processAs("label", ParsedTextType)
        classesOpt <- inv.processAsOpt("class", ParsedTextType)
        classes = "_menuButton btn" +
          classesOpt.map(_.strip.toString).map(" " + _).getOrElse("")
      }
        yield HtmlValue(
          a(
            cls:=classes,
            data.menuid := id.strip.toString(),
            label.strip.toString()))
    }
  }
  
  // TODO: replace this with a QL function! It certainly requires basic math functions, but I'm
  // not sure it needs much else...
  lazy val ShowSomeFunction = new InternalMethod(ShowSomeOID,
    toProps(
      setName("_showSome"),
      Categories(UITag),
      Summary("Show some of the contents at a time"),
      SkillLevel(SkillLevelAdvanced),
      Details("""```
        |THING -> _showSome(START, LEN, MSG, ALL, DISPLAY)
        |```
        |
        |This is a useful but complex function for dealing with long lists. Given the incoming THING,
        |it runs the expression in ALL to compute a bunch of expressions. It produces a LIST of LEN of them
        |with indexes starting at START, feeds that to DISPLAY, and produces the result of that. The whole
        |thing will be finished with MSG; clicking on that produces the next LEN items.
        |
        |The division between ALL and DISPLAY is a bit subtle, and is mainly for efficiency. ALL should
        |contain the code up until the order is clear -- typically until _sort. You *can* put everything
        |into ALL, but it will run more slowly.
        |
        |In the long run, this should be replaced by a cleverer and more automatic mechanism. Also, this
        |may be replaced by a QL function in due course. So consider this experimental; it may go away
        |down the line.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        thing <- inv.contextFirstThing
        start <- inv.processParamFirstAs(0, IntType)
        len <- inv.processParamFirstAs(1, IntType)
        msg <- inv.processParamFirstAs(2, ParsedTextType)
        all <- inv.processParam(3)
        done = (start + len >= all.size)
        rawMsg <- inv.rawParam(2)
        rawAll <- inv.rawParam(3)
        rawDisplay <- inv.rawParam(4)
        selectedElems = all.cType.makePropValue(all.cv.drop(start).take(len), all.pType)
        result <- inv.processParam(4, inv.context.next(selectedElems))
        wiki <- inv.fut(result.wikify(inv.context))
        nextButton =
          if (done)
            ""
          else {
            val nextDiv = s"_nextButton${(scala.math.random * 1000000).toInt.toString()}"
            div(idAttr := nextDiv,
              p(b(a(
                cls := "_qlInvoke",
                msg.raw.toString,
                data.thingId := s"${thing.toThingId}",
                data.target := nextDiv,
                data.noicon := true,
                data.ql := s"_showSome(${start + len},$len,${rawMsg.reconstructStandalone},${rawAll.reconstructStandalone},${rawDisplay.reconstructStandalone})")))
            ).toString
          }
        complete =
          if (all.size == 0)
            Wikitext("")
          else
            wiki + HtmlWikitext(nextButton)
      }
        yield QL.WikitextValue(complete)
    }
  }
  
  lazy val UniqueHtmlId = new InternalMethod(UniqueHtmlIdOID,
    toProps(
      setName("_uniqueHtmlId"),
      SkillLevel(SkillLevelAdvanced),
      Categories(UITag),
      Summary("Generate an ID suitable for using on tags in this page."),
      Details("""It is fairly common, especially when doing complex things with _QLButton, to find that you want a
        |distinct ID for the target of the button, separately for each button on the page. If the _QLButton is
        |being used with a Thing, you can often use `_oid` to create that ID, but otherwise it can be tricky.
        |
        |So the `_uniqueHtmlId` function simply generates a random ID string, suitable for use on the page.
        |
        |You typically use this something like:
        |```
        |My Things -> _foreach(
        |_uniqueHtmlId -> +$displayId
        |\""\[[_QLButton(label=..., target=$displayId, ql=...)\]]
        |...
        |<div id="$displayId"></div>
        |\""
        |)
        |```
        |
        |*Note*: at the moment, these IDs are generated randomly, so they are not absolutely guaranteed to be
        |unique -- there is a very small, but non-zero, chance of an ID collision on the page.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      // The input is actually irrelevant:
      val n = (scala.math.random * Long.MaxValue).toLong
      fut(QL.WikitextValue(models.Wikitext(s"_rndid${n.toString}")))
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
    CreateInstanceLinkMethod,
    CreateButtonFunction,
    QLButton,
    QLLink,
    QLInput,
    ThingTree,
    new MixedButtonMethod,
    ShowSomeFunction,
    MenuButton,
    UniqueHtmlId,
    UpdateableSection
  )
}
