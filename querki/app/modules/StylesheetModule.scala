package modules.stylesheet

// TODO: these are all abstraction breaks!
import play.api.Logger
import play.api.templates.Html
import controllers.PageEventManager

import models._
import models.system.{SystemType, PlainTextType}
import models.system.CommonInputRenderers
import models.system.{LinkModelProp}

import ql._

import querki.ecology._
import querki.util._
import querki.values._

// This is for the PageEventManager and related classes:
import controllers._

object MOIDs extends EcotIds(1) {
  val StylesheetOID = sysId(25)
  val CSSTextOID = sysId(27)
  val CSSOID = sysId(28)
  val StylesheetBaseOID = sysId(29)
  val GoogleFontOID = sysId(32)
}
import models.system.OIDs._

import querki.ql.CodeType

class StylesheetModule(e:Ecology) extends QuerkiEcot(e) {
  import MOIDs._
  
  val Core = initRequires[querki.core.Core]
  val Basic = initRequires[querki.basic.Basic]

  /******************************************
   * TYPES
   ******************************************/
  
  /**
   * A Type for CSS Text as a proper Property, so we can edit directly in Querki.
   */
  class CSSTextType(tid:OID) extends SystemType[String](tid,
    toProps(
        setName("CSS Type"))
    ) with SimplePTypeBuilder[String] with CodeType
  {
    // TODO: filter any Javascript-enabling keywords! This should go in doFromUser().
    
    def doDeserialize(v:String) = v
    def doSerialize(v:String) = v
    def doWikify(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None) = Wikitext(v)

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
      setName("Stylesheet Link"),
      LinkModelProp(StylesheetBase),
      AppliesToKindProp(Kind.Thing),
      Summary("Describes how to render Things when displaying them in the browser"),
      Details("""If you add the Stylesheet Link Property to a Thing, it should point to a
          |Stylesheet whose CSS describes some page layout. That CSS will be used when displaying
          |this Thing.
          |
          |If you set a Stylesheet on a Model, it will be used for all Instances of that Model.
          |
          |If you set a Stylesheet on a Space, it will be used for all Things in that Space. This
          |is the easiest way to style the entire Space.""".stripMargin)
      ))

  /**
   * This special property is used for Stylesheet Things. Basically, if this Thing is used as
   * the Stylesheet for other Things, it should have this Property set.
   */
  lazy val CSSProp = new SystemProperty(CSSOID, CSSTextType, Optional,
    toProps(
      setName("CSS"),
      Core.InternalProp(true),
      SkillLevel(SkillLevelAdvanced),
      Summary("The actual CSS for a Stylesheet"),
      Details("""This is the main property on a Stylesheet. It can contain more or less
          |any arbitrary CSS, with just a few security-related exceptions.
          |
          |For the time being, we're not providing any fancy UI for making it easier to write
          |CSS. This will likely happen someday, but for now, you should only use this property
          |if you are comfortable with CSS and know what you are doing. (We recommend editing
          |the Stylesheet in one tab, and having a page that uses it in another -- this makes it
          |quick and easy to check your work as you go.)
          |
          |Note that Querki is based on Twitter Bootstrap -- you can assume that all Bootstrap
          |styles are already available.
          |
          |You should not usually add this Property to Things. Instead, create a
          |Stylesheet, and edit the CSS Property on that.""".stripMargin)
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
      AppliesToKindProp(Kind.Thing),
      Summary("The name of a Google Font to use in these styles"),
      Details("""Google provides a [large number of webfonts](http://www.google.com/fonts/) for public use.
          |We find them useful, so we've made them available through Querki.
          |
          |To use a Google font, just add this Property to your Stylesheet, and set it to the name of the
          |font you would like to use. You may then use it as a font in the Stylesheet's CSS like any other.
          |
          |For example, if you add Google's 'Tangerine' font to your stylesheet, you can then say something like
          |    p {
          |      font-family: 'Tangerine', serif;
          |    }
          |to use that font for all normal paragraphs.
          |
          |NOTE: this feature is experimental, and likely to change. In the future, we will probably add the
          |ability to list multiple fonts instead of just one.""".stripMargin)
      ))
  
  override lazy val props = Seq(
    StylesheetProp,
    CSSProp,
    GoogleFontProp
  )
  
  /***********************************************
   * THINGS
   ***********************************************/

  lazy val StylesheetBase = new ThingState(StylesheetBaseOID, systemOID, querki.basic.MOIDs.SimpleThingOID,
    toProps(
      setName("Stylesheet"),
      Core.IsModelProp(true),
      Basic.DisplayTextProp("[[_code(CSS)]]"),
      CSSProp("")))
  
  override lazy val things = Seq(StylesheetBase)
  
  object HeaderHandler extends Contributor[HtmlEvent, String] {
    
    def stylesheetsForThing(state:SpaceState, thing:Thing):String = {
      implicit val s = state
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
