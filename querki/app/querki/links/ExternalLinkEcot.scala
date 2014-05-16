package querki.links

import models.{ThingState, Wikitext}

import querki.core.URLableType
import querki.ecology._
import querki.types.{ModeledPropertyBundle, SimplePropertyBundle}
import querki.values.{ElemValue, QLContext, SpaceState}

object ExternalLinkMOIDs extends EcotIds(46) {
  val ExternalLinkTypeOID = moid(1)
  val ExternalLinkModelOID = moid(2)
  val ExternalLinkUrlOID = moid(3)
  val WithParamFunctionOID = moid(4)
}

class ExternalLinkEcot(e:Ecology) extends QuerkiEcot(e) with querki.core.MethodDefs with querki.types.ModelTypeDefiner {
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
    
    override def doWikify(context:QLContext)(bundle:ModeledPropertyBundle, displayOpt:Option[Wikitext] = None) = {
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
  
  lazy val ExternalLinkUrlProp = new SystemProperty(ExternalLinkUrlOID, Links.OldExternalLinkType, ExactlyOne,
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

  override lazy val props = Seq(
    ExternalLinkUrlProp,
    WithParamFunction
  )
}