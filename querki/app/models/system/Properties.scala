package models.system

import play.api.Logger

import models._

import Property._
import Thing._
import YesNoType._

import OIDs._
import SystemSpace._

import querki.core

import querki.ecology._
import querki.types._
import querki.values._

// TODO: replace this with the version in QuerkiEcot:
class SystemProperty[VT, -RT](pid:OID, t:PType[VT] with PTypeBuilder[VT, RT], c:Collection, p:PropFetcher)(implicit e:Ecology = querki.ecology.Ecology) 
  extends Property[VT, RT](pid, systemOID, core.MOIDs.UrPropOID, t, c, p, querki.time.epoch)(e)
  
// TEMP: while we're getting things working:
object Summary {
  def apply(text:String) = (querki.conventions.MOIDs.PropSummaryOID -> ExactlyOne(TextType(text))) 
}
object Details {
  def apply(text:String) = (querki.conventions.MOIDs.PropDetailsOID -> ExactlyOne(LargeTextType(text)))
}
object NotInherited {
  def apply() = (querki.core.MOIDs.NotInheritedOID -> ExactlyOne(YesNoType(true)))
}
object SkillLevel {
  val Basic = querki.identity.skilllevel.MOIDs.SkillLevelBasicOID
  val Standard = querki.identity.skilllevel.MOIDs.SkillLevelStandardOID
  val Advanced = querki.identity.skilllevel.MOIDs.SkillLevelAdvancedOID
  def apply(level:OID) = (querki.identity.skilllevel.MOIDs.SkillLevelPropOID -> ExactlyOne(LinkType(level)))
}
object AppliesToKindProp {
  def apply(kind:Kind.Kind) = (querki.core.MOIDs.AppliesToKindOID -> ExactlyOne(IntType(kind)))
}
object InternalProp {
  def apply(b:Boolean) = (querki.core.MOIDs.InternalPropOID -> ExactlyOne(YesNoType(b)))
}
    
  object PlaceholderTextProp extends SystemProperty(PlaceholderTextOID, PlainTextType, Optional,
      toProps(
        setName("Placeholder Text"),
        AppliesToKindProp(Kind.Property),
        (querki.basic.MOIDs.DeprecatedOID -> true),
        Summary("Placeholder text for input boxes"),
        Details("""In Text Properties, it is often helpful to have a prompt that displays inside the input
            |field until the user begins to type something there. If the Property has a Placeholder Text, that
            |will be displayed in grey when the input is first shown.""".stripMargin)
        ))
  
  object PromptProp extends SystemProperty(PromptOID, PlainTextType, Optional,
      toProps(
        setName("Prompt"),
        AppliesToKindProp(Kind.Property),
        Summary("Prompt to use in the Editor"),
        Details("""In the Editor, Properties are usually displayed with their Name. If you want to show something
            |other than the Name, set the Prompt Property to say what you would like to show instead.""".stripMargin)
        ))

/**
 * Meta-property, set on Properties of LinkType, to filter what to Link to.
 */
object LinkKindProp extends SystemProperty(LinkKindOID, IntType, QList,
    toProps(
      setName("Link Kind"),
      SkillLevel(SkillLevel.Advanced),
      Summary("The Kind that this Property can Link to"),
      Details("""When you create a Link Property, if you do *not* set the *Link Model* Property on it,
          |you may want to at least specify which *Kind* of Thing this can Link to. There are five Kinds of
          |Things in Querki:
          |
          |* Ordinary Thing
          |* Property
          |* Space
          |* Type
          |* Collection
          |
          |99% of the time, you will want to link to ordinary Things. (And most of those times, you should set
          |a particular Link Model.) Occasionally, for very complex systems, you may want to Link to Property
          |or Space instead. You are not likely to ever link to Type or Collection, but it is possible to do so.
          |
          |This is an extremely advanced property, and not intended for casual use.""".stripMargin),
      AppliesToKindProp(Kind.Property)
      ))

object LinkAllowAppsProp extends SystemProperty(LinkAllowAppsOID, YesNoType, Optional,
    toProps(
      setName("Allow Links to Apps"),
      Summary("Should this Property allow Links to Things in Apps?"),
      Details("""Normally, links are only to other Things in this Space. But if this flag is set, this
          |says that this Property should allow linking to Things in Apps of this Space.
          |
          |This is an advanced property, and not intended for casual use.""".stripMargin),
      AppliesToKindProp(Kind.Property),
      SkillLevel(SkillLevel.Advanced)
      ))

object LinkModelProp extends SystemProperty(LinkModelOID, LinkType, Optional,
    toProps(
      setName("Link Model"),
      Summary("Which Things can this Property link to?"),
      Details("""By default, Link Properties allow you to link to *anything*. This usually isn't what
          |you want, though -- most often, you're looking for Instances of a specific Model. For example,
          |if you specify the Stylesheet Property, you only want to give Stylesheets as options to Link to:
          |it would be meaningless to have the Stylesheet point to something like a recipe or a to-do list.
          |
          |So this is a meta-Property: when you create a Property that is a Link, you can add this to say
          |exactly what it can link *to*. It is strongly recommended that you set this on all Link Properties
          |you create -- it makes them easier to use, and tends to prevent confusing errors.
          |
          |Note that this is only enforced loosely, and you can't absolutely count upon this restriction
          |always being true. But used properly, it will steer folks in the right direction.""".stripMargin),
      AppliesToKindProp(Kind.Property),
      LinkToModelsOnlyProp(true)
      ))

// TODO: As it says, replace this with a more general Link Filter property. That will need bigger
// refactorings, though: I started to build that, only to discover that SpaceState.linkCandidates
// doesn't have all the request-context information needed to resolve a QL Expression.
object LinkToModelsOnlyProp extends SystemProperty(LinkToModelsOnlyOID, YesNoType, ExactlyOne,
    toProps(
      setName("Link to Models Only"),
      (querki.identity.skilllevel.MOIDs.SkillLevelPropOID -> ExactlyOne(LinkType(querki.identity.skilllevel.MOIDs.SkillLevelAdvancedOID))),
      Summary("Only allow this Property to Link to Models"),
      Details("""If set to true, this Link Property will only show Models as options to link to in the editor.
          |
          |This is an advanced property, and something of a hack -- don't get too comfortable with it. In the
          |medium term, it should get replaced by a more general LinkFilter property that lets you specify which
          |Things to link to.""".stripMargin)))

// TODO: this should really only allow the properties that are defined on this Model:
object InstanceEditPropsProp extends SystemProperty(InstanceEditPropsOID, LinkType, QList,
    toProps(
      setName("Properties to edit in Instances"),
      LinkAllowAppsProp(true),
      LinkKindProp(Kind.Property),
      Summary("Which Properties should be edited in Instances of this Model?"),
      Details("""It is very common to define a bunch of Properties on a Model that you really don't
          |ever intend to change on the Instances. (In particular, you very often will define the Display
          |Text on the Model, not on the Instances.) This results in your Instance Editor being cluttered
          |with lots of Properties that you never, ever use.
          |
          |So this Property is a quick-and-easy way to lay out your Instance Editor. It is a List of
          |Properties that you can define however you like. When you create or edit an Instance of this
          |Model, it will display exactly those Properties, in that order, which usually makes it
          |easier for you to write your Instances.
          |
          |BUG NOTE: this doesn't immediately register when you've added a Property to the Model, so it
          |doesn't list the newly-added Property. For now, after you add a Property, save the Model and then
          |edit it again -- the Property should now show up for you to use.""".stripMargin))) with LinkCandidateProvider
{
  def getLinkCandidates(state:SpaceState, currentValue:DisplayPropVal):Seq[Thing] = {
    currentValue.on match {
      case Some(thing) => {
        // We're applying this to some actual thing, so list its Properties as options:
        thing.allProps(state).toSeq.sortBy(_.displayName)
      }
      case _ => Seq.empty
    }
  }
}

object ShowUnknownProp extends SystemProperty(ShowUnknownOID, LargeTextType, ExactlyOne,
    toProps(
      setName("Undefined Tag View"),
      AppliesToKindProp(Kind.Space),
      Summary("What should be displayed when you click on a Tag that isn't a Thing?"),
      Details("""In Querki, it is entirely legal to refer to the name of something you haven't written yet --
          |for instance, Tags are often names with no definition. So the question becomes, what should be
          |displayed when you click on one of these tags, since it doesn't point to a real Thing?
          |
          |The Undefined Tag View Property defines that. It is a Large Text that is defined on the Space; when
          |you try to look at an unknown name, it will show this text. You can put QL expressions in here; they
          |will receive the Name that you are trying to look at.
          |
          |You can also put an Undefined Tag View on a Model, which basically means that all Tags of this Model
          |will use that View. (Technically, this means all Tags that are used in a Tag Set whose Link Model
          |points to this Model.)
          |
          |There is a simple default value that is defined on every Space by default. But you should feel free
          |to override that to do something more interesting, especially if you are doing interesting things
          |with Tags in your Space.""".stripMargin)))

object NoCreateThroughLinkProp extends SystemProperty(NoCreateThroughLinkOID, YesNoType, ExactlyOne,
    toProps(
      setName("No Create Through Link Model"),
      NotInherited(),
      Summary("Set this to prevent new instances from being created accidentally."),
      Details("""When you create a Link Property in the Editor, you can set the "Link Model" -- the sort of Thing
          |that this Property points to. The Editor then lets you choose from all of the existing Instances of that
          |Model, and also lets you create a new one.
          |
          |Sometimes, though, you don't want to create any new ones from the Editor. In particular, if you've already
          |created all of the Instances of this Model that you ever expect to want, then it is simply annoying to have
          |that option. In that case, put this Property on your Model, and set it to True -- it will make that option
          |in the Editor go away.""".stripMargin)))
