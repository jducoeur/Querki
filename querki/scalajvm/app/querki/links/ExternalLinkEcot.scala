package querki.links

import scala.concurrent.Future

import models.{PropertyBundle, ThingState, Wikitext}

import querki.core.URLableType
import querki.globals._
import querki.ecology._
import querki.types.{ModeledPropertyBundle, SimplePropertyBundle}
import querki.util.QLog
import querki.values.{ElemValue, QLContext, SpaceState}

object ExternalLinkMOIDs extends EcotIds(46) {
  val ExternalLinkTypeOID = moid(1)
  val ExternalLinkModelOID = moid(2)
  val ExternalLinkUrlOID = moid(3)
  val WithParamFunctionOID = moid(4)
  val OIDLinkOID = moid(5)
}

class ExternalLinkEcot(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs with querki.types.ModelTypeDefiner with EcologyMember {
  import ExternalLinkMOIDs._
    
  val Basic = initRequires[querki.basic.Basic]
  val Editor = initRequires[querki.editing.Editor]
  val Links = initRequires[Links]

  /***********************************************
   * TYPES
   ***********************************************/
  
  lazy val ExternalLinkModel = ThingState(ExternalLinkModelOID, systemOID, RootOID,
    toProps(
      setName("_externalLinkModel"),
      setInternal,
      Summary("The Model underlying the External Link Type."),
      ExternalLinkUrlProp(),
      Basic.DisplayNameProp(),
      Editor.InstanceProps(ExternalLinkUrlProp, Basic.DisplayNameProp)))
      
  override lazy val things = Seq(
    ExternalLinkModel
  )
  
  lazy val ExternalLinkType = new ModelType(ExternalLinkTypeOID, ExternalLinkModelOID,
    toProps(
      setName("External Link Type"),
      Basic.ExplicitProp(true),
      Summary("A proper link to an external website, including both the URL and display text.")))
    with URLableType
  {
    def getURL(context:QLContext)(elem:ElemValue):Option[String] = {
      implicit val s = context.state
      val bundle = elem.get(this)
      for {
        pv <- bundle.getPropOpt(ExternalLinkUrlProp)
        url <- pv.firstOpt
      }
        yield url.url
    }
    def getDisplay(context:QLContext)(elem:ElemValue):Option[String] = {
      implicit val s = context.state
      val bundle = elem.get(this)
      for {
        pv <- bundle.getPropOpt(Basic.DisplayNameProp)
        text <- pv.firstOpt
      }
        yield text.text      
    }
    
    override def doWikify(context:QLContext)(bundle:ModeledPropertyBundle, displayRawOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = {
      implicit val s = context.state
      
      val urlOpt = for {
        pv <- bundle.getPropOpt(ExternalLinkUrlProp)
        url <- pv.firstOpt
      }
        yield url.url
        
      val displayNameOpt = for {
        pv <- bundle.getPropOpt(Basic.DisplayNameProp)
        text <- pv.firstOpt
      }
        yield text.text
        
      val displayOpt = displayRawOpt.map(_.raw.toString())

      Future.successful(displayOpt.orElse(displayNameOpt).orElse(urlOpt).map(display => Wikitext("[" + display + "](" + urlOpt.getOrElse("#") + ")")).getOrElse(Wikitext.empty))
    }
  }
  
  override lazy val types = Seq(
    ExternalLinkType
  )
      
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val ExternalLinkUrlProp = new SystemProperty(ExternalLinkUrlOID, Links.URLType, ExactlyOne,
    toProps(
      setName("_url"),
      setInternal,
      Editor.PromptProp("URL"),
      Summary("The URL of a webpage, usually outside Querki"),
      Details("This is usually used inside of an External Link Type Property.")))
      
  /***********************************************
   * FUNCTIONS
   ***********************************************/  

  lazy val WithParamFunction = new InternalMethod(WithParamFunctionOID,
    toProps(
      setName("_withParam"),
      Summary("Adds the specified query parameter to a Link or URL"),
      Details("""    LINK or URL -> _withParam(PARAMNAME, VALUE, RAW) -> URL
        |
        |By and large, the VALUE is translated in such a way that, if the LINK points to a Querki page,
        |the page can use that parameter as $PARAMNAME in QL expressions.
        |
        |The RAW parameter is optional, and defaults to false. If you set it to true, this will omit
        |quotes around the parameter if it is text. You should generally only do this if this is going to an external
        |page; omitting the quotes when sending to a Querki page will usually result in the parameter not
        |getting interpreted correctly.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      for {
        pt <- inv.contextTypeAs[URLableType]
        elemContext <- inv.contextElements
        elemV <- inv.opt(elemContext.value.firstOpt)
        urlStr <- inv.opt(pt.getURL(elemContext)(elemV))
        displayStr <- inv.opt(pt.getDisplay(elemContext)(elemV))
        paramNameElem <- inv.processParam(0, elemContext)
	      paramName = awaitHack(paramNameElem.wikify(elemContext)).raw.str
        valElem <- inv.processParam(1, elemContext)
        raw <- inv.processParamFirstOr(2, YesNoType, false, elemContext)
        value = vToParam(valElem, elemContext, raw)(inv.state)
      }
        yield ExactlyOne(ExternalLinkType(appendParam(urlStr, displayStr, paramName, value)))
    }
    
    def vToParam(v:QValue, context:QLContext, raw:Boolean)(implicit state:SpaceState):String = {
      querki.util.SafeUrl(v.pType.toUrlParam(v.first, raw))
    }
    
    def appendParam(url:String, display:String, paramName:String, paramVal:String):SimplePropertyBundle = {
      if (url.contains("?"))
        SimplePropertyBundle(
          ExternalLinkUrlProp(url + "&" + paramName + "=" + paramVal),
          Basic.DisplayNameProp(display))
      else
        SimplePropertyBundle(
          ExternalLinkUrlProp(url + "?" + paramName + "=" + paramVal),
          Basic.DisplayNameProp(display))
    }
  }
  
  lazy val OIDLinkFunction = new InternalMethod(OIDLinkOID, 
    toProps(
      setName("_oidLink"),
      SkillLevel(SkillLevelAdvanced),
      Summary("Get the OID Link from a Thing"),
      Details("""    THING -> _oidLink -> External Link
          |
          |Most of the time, you create a link to a Thing simply by naming the Thing. The resulting link is by the
          |"ThingId" -- the Link Name if it has one, otherwise the OID. Occasionally, though, you may want to specifically
          |link via OID. (For example, if you think the Link Name might change out from under you.) That is what
          |_oidLink is for: it takes the Thing and produces an External Link using the OID.
          |
          |You may then feed this into something like _iconButton in order to display the
          |resulting Link as a button, if you like.
          |
          |Keep in mind that this is rarely needed, though. Most of the time, when you want to display the link
          |to a passed-in Thing, you don't even need a QL expression -- you can simply say \____ (four underscores)
          |in your QText, and that translates as "display the received context". If you've passed in a Thing (which
          |you most often do), that will display as an HTML link to that Thing, showing its Display Name.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      for {
        t <- inv.contextAllThings
      }
        yield 
          ExactlyOne(
            ExternalLinkType(
              SimplePropertyBundle(
                ExternalLinkUrlProp(t.id.toThingId),
                Basic.DisplayNameProp(t.displayName))))
    }
  }

  override lazy val props = Seq(
    ExternalLinkUrlProp,
    WithParamFunction,
    OIDLinkFunction
  )
}
