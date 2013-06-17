package modules.stylesheet

import play.api.Logger
import play.api.templates.Html

import models._
import models.Space.oidMap
import models.system._
// We need this because some of these objects were originally created in System,
// and their OIDs live there:
import models.system.OIDs.sysId
import models.Thing._

import ql._

// This is for the PageEventManager and related classes:
import controllers._

object OIDs {
  val StylesheetOID = sysId(25)
  val CSSTextOID = sysId(27)
  val CSSOID = sysId(28)
  val StylesheetBaseOID = sysId(29)
  val GoogleFontOID = sysId(32)
}
import models.system.OIDs._
import OIDs._

class StylesheetModule(val moduleId:Short) extends modules.Module {
  
  /******************************************
   * TYPES
   ******************************************/
  
  /**
   * A Type for CSS Text as a proper Property, so we can edit directly in Querki.
   */
  class CSSTextType(tid:OID) extends SystemType[String](tid,
    toProps(
        setName("Type-CSS"))
    ) with SimplePTypeBuilder[String] with CodeType
  {
    // TODO: filter any Javascript-enabling keywords! This should go in doFromUser().
    
    def doDeserialize(v:String) = v
    def doSerialize(v:String) = v
    def doRender(context:ContextBase)(v:String) = Wikitext(v)

    val doDefault = ""
    
    override def renderInputXml(prop:Property[_,_], state:SpaceState, currentValue:DisplayPropVal, v:ElemValue):scala.xml.Elem =
      CommonInputRenderers.renderLargeText(prop, state, currentValue, v, this)
      
    def code(elem:ElemValue):String = get(elem)
  }
  lazy val CSSTextType = new CSSTextType(CSSTextOID)  
  override lazy val types = Seq(CSSTextType)
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  /**
   * Points to the optional CSS file for this Thing. If placed on a Space, applies Space-wide.
   */
  lazy val StylesheetProp = new SystemProperty(StylesheetOID, LinkType, Optional,
    toProps(
      setName("Stylesheet"),
      LinkModelProp(StylesheetBase),
      AppliesToKindProp(Kind.Thing)
      ))

  /**
   * This special property is used for Stylesheet Things. Basically, if this Thing is used as
   * the Stylesheet for other Things, it should have this Property set.
   */
  lazy val CSSProp = new SystemProperty(CSSOID, CSSTextType, Optional,
    toProps(
      setName("CSS")
      ))

  /**
   * This is the name of a Google Font to embed. It should be referenced from a Stylesheet.
   * 
   * TODO: this probably shouldn't be TextType, but some more limited type that only allows
   * a small character set.
   * 
   * TODO: this should probably be a List instead of just a single item, so you can specify
   * multiple fonts.
   */
  lazy val GoogleFontProp = new SystemProperty(GoogleFontOID, PlainTextType, Optional,
    toProps(
      setName("Google Font Name"),
      // TODO: in fact, this only applies to Stylesheets:
      AppliesToKindProp(Kind.Thing)
      ))
  
  override lazy val props = Seq(
    StylesheetProp,
    CSSProp,
    GoogleFontProp
  )
  
  /***********************************************
   * THINGS
   ***********************************************/

  lazy val StylesheetBase = new ThingState(StylesheetBaseOID, systemOID, RootOID,
    toProps(
      setName("Stylesheet"),
      IsModelProp(true),
      DisplayTextProp("[[_code(CSS)]]"),
      CSSProp("")))
  
  override lazy val things = Seq(StylesheetBase)
  
  object HeaderHandler extends Contributor[HtmlEvent, String] {
    
    def stylesheetsForThing(state:SpaceState, thing:Thing):String = {
      implicit val s = state
      Logger.info("Prepping Stylesheet for " + thing.displayName)
      val parentStylesheetStr = if (thing.hasModel) stylesheetsForThing(state, thing.getModel) else ""
      val localStylesheetOpt =
        for (propVal <- thing.getPropOpt(StylesheetProp);
             stylesheetOID = propVal.first)
          yield state.thing(stylesheetOID)
      val localStylesheetStr = localStylesheetOpt.map(stylesheet => "<style type=\"text/css\">" + stylesheet.first(CSSProp) + "</style>")
      val googleFontStr =
        for (
          stylesheet <- localStylesheetOpt;
          fontProp <- stylesheet.getPropOpt(GoogleFontProp);
          fonts = fontProp.first.raw.toString()
            )
          yield "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://fonts.googleapis.com/css?family=" + fonts + "\">"
      parentStylesheetStr + localStylesheetStr.getOrElse("") + googleFontStr.getOrElse("")
    }
    
    def notifyGuts(rc:RequestContext):String = {
      val result = 
        for (state <- rc.state;
             thing <- rc.thing)
          // TBD: Is this going to walk up the App chain properly? Might need enhancement 
          yield stylesheetsForThing(state, state) + stylesheetsForThing(state, thing)
      result.getOrElse("")
    }
    
    def notify(evt:HtmlEvent, sender:Publisher[HtmlEvent,String]):String = {
      // For the time being, only Thing pages show styles. This will probably change
      if (evt.template == QuerkiTemplate.Thing)
        notifyGuts(evt.rc)
      else
        ""
    }
  }
  
  override def init = {
    PageEventManager.addHeaders.subscribe(HeaderHandler)
  }
  
  override def term = {
    PageEventManager.addHeaders.unsubscribe(HeaderHandler)
  }
}
