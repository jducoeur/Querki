package querki.links

import models.{PropertyBundle, ThingState, Wikitext}

import querki.core.URLableType
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
    
    override def doWikify(context:QLContext)(bundle:ModeledPropertyBundle, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = {
      implicit val s = context.state
      
      val urlOpt = for {
        pv <- bundle.getPropOpt(ExternalLinkUrlProp)
        url <- pv.firstOpt
      }
        yield url.url
        
      val displayOpt = for {
        pv <- bundle.getPropOpt(Basic.DisplayNameProp)
        text <- pv.firstOpt
      }
        yield text.text      

      displayOpt.orElse(urlOpt).map(display => Wikitext("[" + display + "](" + urlOpt.getOrElse("#") + ")")).getOrElse(Wikitext.empty)
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
      Details("""    LINK or URL -> _withParam(PARAMNAME, VALUE) -> URL""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      for {
        pt <- inv.contextTypeAs[URLableType]
        elemContext <- inv.contextElements
        elemV <- inv.opt(elemContext.value.firstOpt)
        urlStr <- inv.opt(pt.getURL(elemContext)(elemV))
        displayStr <- inv.opt(pt.getDisplay(elemContext)(elemV))
        paramNameElem <- inv.processParam(0, elemContext)
	    paramName = paramNameElem.wikify(elemContext).raw.str
        valElem <- inv.processParam(1, elemContext)
        value = vToParam(valElem, elemContext)(inv.state)
      }
        yield ExactlyOne(ExternalLinkType(appendParam(urlStr, displayStr, paramName, value)))
    }
    
    def vToParam(v:QValue, context:QLContext)(implicit state:SpaceState):String = {
      // We really want to serialize the value, so that _asType() will work on the receiving end,
      // but we can't reliably do so -- many types don't implement serialization.
      // TBD: should there be an asURLParam method on PType? What's the right way to deal with this
      // problem?
      try {
        v.pType.serialize(v.first)
      } catch {
        case ex:Exception => v.wikify(context).raw.str
      }
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
