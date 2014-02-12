package querki.basic

import scala.xml.NodeSeq

import models._

import querki.conventions._
import querki.core._
import querki.ecology._
import querki.ql.QLPhrase
import querki.types._
import querki.values.{ElemValue, QLContext, RequestContext, SpaceState}

class BasicModule(e:Ecology) extends QuerkiEcot(e) with Basic with TextTypeBasis with PlainTextBaseType {
  import MOIDs._
  
  val DeriveName = initRequires[querki.types.DeriveName]
  val Types = initRequires[querki.types.Types]
  
  lazy val IsModelProp = Core.IsModelProp
  
  /***********************************************
   * TYPES
   ***********************************************/
  
  lazy val PlainTextType = new PlainTextType(PlainTextOID, "Plain Text Type") 
  {
    override def editorSpan(prop:Property[_,_]):Int = 6
  }

  /**
   * A QL field is sort of like inside-out QLText. It is processed very similarly,
   * but whereas the "outer" layer of QLText is expected to be QText, with QL in
   * subclauses, the outer layer of a QL field is QL, with wikitext in subclauses.
   * 
   * In other words, it is like QLText, but just the stuff inside the [[ ]] parts.
   * 
   * QL fields are also processed a bit differently. QLText is fully processed and
   * rendered, producing QText. QL fields are essentially methods, which get *called*
   * from other methods and from QLText. So the results are not turned directly into
   * QText; instead, the resulting Context is fed back out to the caller.
   * 
   * The public Name for this is now Function, because really, that's what it is. It
   * now is getting powerful enough to be worth the name.
   */
  lazy val QLType = new TextTypeBase(QLTypeOID,
    toProps(
      setName("Function")
    )) with PTypeBuilder[QLText,String] 
  {
    override def editorSpan(prop:Property[_,_]):Int = 12   
  
    override def renderInputXml(prop:Property[_,_], rc:RequestContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq =
      renderLargeText(prop, rc, currentValue, v, this)

    // TBD: in principle, we really want this to return a *context*, not a *value*. This is a special
    // case of a growing concern: that we could be losing information by returning QValue from
    // qlApply, and should actually be returning a full successor Context.
    override def qlApplyFromProp(definingContext:QLContext, incomingContext:QLContext, prop:Property[QLText,_], params:Option[Seq[QLPhrase]]):Option[QValue] = {
      if (definingContext.isEmpty) {
        Some(interface[querki.ql.QL].WarningValue("""Trying to use QL Property """" + prop.displayName + """" in an empty context.
This often means that you've invoked it recursively without saying which Thing it is defined in."""))
      } else {
        Some(prop.applyToIncomingThing(definingContext) { (thing, context) =>
          val qlPhraseText = thing.first(prop)(context.state)
          QL.processMethod(qlPhraseText, incomingContext.forProperty(prop), params)
        })
      }
    }
  }
  
  override lazy val types = Seq(
    PlainTextType,
    QLType
  )
  
  def TextValue(msg:String):QValue = ExactlyOne(PlainTextType(msg))
  
  /***********************************************
   * PROPERTIES
   ***********************************************/

  // TODO: is there any reason this needs to go in Core? I think both this and QLType itself can go into Basic:
  lazy val ApplyMethod = new SystemProperty(ApplyMethodOID, QLType, Optional,
    toProps(
      setName("_apply"),
      SkillLevel(SkillLevelAdvanced),
      Summary("A QL Expression that will be run when you name this Thing."),
      Details("""_apply is an advanced function, and most users will not use it directly. But it is probably
          |the most important Property in Querki, and advanced users may want to play with it.
          |
          |One of Querki's design goals was that it should Just Work. This is reflected, more than anywhere else,
          |in the fact that you can just say:
          |[[_code(""[[My Thing]]"")]]
          |and it shows up as a pointer to *My Thing*.
          |
          |That seems obvious, but consider -- you can also say:
          |[[_code(""[[My Property]]"")]]
          |and what you get isn't a pointer to *My Property* -- instead, you get the *value* of My Property on the
          |Thing you're looking at.
          |
          |Moreover, you can say:
          |[[_code(""[[All Things]]"")]]
          |on a page, and what you get is a listing of all of the Things in this Space! So what the heck is going on
          |here?
          |
          |The secret behind the magic is the _apply method. _apply is a Property that is defined on *every* Thing.
          |(More or less -- system-defined Things use a closely-related built-in mechanism.) It defines exactly
          |"What should happen when I name this Thing?" So _apply on Properties displays the value of the Property
          |on the received Thing; _apply on All Things is this QL Expression:
          |[[_code(All Things._apply)]]
          |And _apply for Thing (the Model that everything is based on) simply produces a pointer to this thing.
          |
          |You can define _apply for your own Things as well -- indeed, the way you usually write your own serious
          |Methods is to define a Thing that just has an _apply Property, and then you can use the Method just like
          |the system-defined ones, by name.
          |
          |The QL Expression in the _apply Property will receive whatever is passed in, and should produce whatever
          |you want to pass out. It is currently completely unstructured and untyped. However, note that we will
          |probably be moving towards more structure in the future, and you should always try to be consistent:
          |as with any QL Expression, you should expect to receive a specific Type, and always produce a specific Type.""".stripMargin)))

  /**
   * If set, this is the display name of the specified object. Whereas the primary NameProp
   * has a number of restrictions, the DisplayNameProp does not. It is used to list a Thing
   * by preference when it is set.
   */
  lazy val DisplayNameProp = new SystemProperty(DisplayNameOID, PlainTextType, Optional,
    toProps(
      setName("Display Name"),
      NotInherited,
      Types.MinTextLengthProp(1),
      Summary("How to show this Thing's Name"),
      Details("""Most Things in Querki have a Name. (It isn't strictly required, but strongly encouraged most
          |of the time.) In general, when we list a Thing, we show its Name. However, if you want to display
          |something *other* than its Name instead, set its Display Name Property to show in its place.
          |
          |Display Name is mainly useful when the name you would like to use includes characters that aren't
          |legal in Names, such as quotes, apostrophes, commas or other punctuation characters.
          |
          |Note that the relationship of Name and Display Name is still in some flux, and things may shift a
          |bit over time. We are thinking of putting Display Name more front-and-center, and making Name derive
          |from that instead.""".stripMargin)
      ))
  
  // TODO: the name DisplayTextProp still need to be renamed to DefaultViewProp:
  lazy val DisplayTextProp = new SystemProperty(DisplayTextOID, LargeTextType, Optional,
      toProps(
        setName("Default View"),
        Summary("How this Thing will be displayed"),
        Details("""Default View is one of the most important Properties in Querki,
        		|and nearly every Thing has one. The Default View describes how this Thing will usually show up when you
        		|look at it as a web page. It can say almost anything you like, but usually consists of a mix of
        		|text and QL expressions. (Where a "QL Expression" is anything inside double-square-brackets.)""".stripMargin)
        ))

  lazy val DeprecatedProp = new SystemProperty(DeprecatedOID, YesNoType, ExactlyOne,
    toProps(
      setName("Deprecated"),
      NotInherited,
      SkillLevel(SkillLevelAdvanced),
      Summary("True iff this Thing is Deprecated."),
      Details("""This is a marker flag that you can put on a Thing to say that it is on its way out, and shouldn't
          |be used any more.
          |
          |The exact meaning of Deprecated depends on the situation, but Querki will tend to hide Things marked as
          |Deprecated. If you see somewhere that a Deprecated Thing is visible and shouldn't be, please log a bug
          |report about it.""".stripMargin)))

  override lazy val props = Seq(
    ApplyMethod,
    DisplayNameProp,
    DisplayTextProp,
    DeprecatedProp
  )
  
  /***********************************************
   * THINGS
   ***********************************************/

  lazy val SimpleThing = ThingState(SimpleThingOID, systemOID, RootOID,
    toProps(
      setName("Simple-Thing"),
      IsModelProp(true),
      DisplayTextProp(Core.QNone),
      (querki.basic.MOIDs.DisplayNameOID -> Core.QNone),
      DeriveName.DeriveNameProp(DeriveName.DeriveInitially)))

  lazy val Page = ThingState(PageOID, systemOID, SimpleThingOID,
    toProps(
      setName("Simple-Page"),
      IsModelProp(true),
      DeprecatedProp(true)))

  lazy val PhotoBase = ThingState(PhotoBaseOID, systemOID, querki.basic.MOIDs.SimpleThingOID,
    toProps(
      setName("Photograph-Base"),
      IsModelProp(true),
      DisplayTextProp("""
This is the Model for all uploaded photographs. You shouldn't try to base something on this directly --
just upload a photograph, and you'll get one of these.
""")),
    querki.time.epoch,
    Kind.Attachment
    )

object Bulleted extends ThingState(BulletedOID, systemOID, RootOID,
    toProps(
      setName("_bulleted"),
      ApplyMethod("\"\"* ____\"\""),
      DisplayTextProp("""    LIST -> _bulleted
          |This method takes a LIST, and render its elements as a bullet list, one per line. It is simply syntactic sugar for
          |    LIST -> \""* \____\"" """.stripMargin)))

object Commas extends ThingState(CommasMethodOID, systemOID, RootOID,
    toProps(
      setName("_commas"),
      ApplyMethod("""_join("", "")"""),
      DisplayTextProp("""    LIST -> _commas
          |This method takes a LIST, and render its elements comma-separated. It is simply syntactic sugar for
          |    LIST -> _join(\"", \"")""".stripMargin)))

object DisplayThingTree extends ThingState(DisplayThingTreeOID, systemOID, RootOID,
    toProps(
      setName("_displayThingTree"),
      ApplyMethod("""""[[_if(_isModel, ""{{_modelInTree:"")]]____[[_if(_isModel, "" [[_createInstanceLink -> _iconButton(""icon-plus-sign"", ""Create an Instance"")]]}}"")]]
{{indent:[[_children -> 
  _sort -> 
  _displayThingTree]]
}}
""""")))

object AllThings extends ThingState(AllThingsOID, systemOID, RootOID,
    toProps(
      setName("All Things"),
      DisplayTextProp("[[All Things]]"),
      ApplyMethod("""""{{_thingTree:
[[_currentSpace ->
  _externalRoots ->
  _sort ->
  _displayThingTree]]
}}""""")))

object AllProps extends ThingState(AllPropsThingOID, systemOID, RootOID,
    toProps(
      setName("All Properties"),
      DisplayTextProp("[[All Properties]]"),
      ApplyMethod("""""{{_thingTree:
[[_currentSpace ->
  _allProps ->
  _bulleted]]
}}
""""")))

  override lazy val things = Seq(
    SimpleThing,
    Page,
    PhotoBase,
    Bulleted,
    Commas,
    DisplayThingTree,
    AllThings,
    AllProps
  )
}