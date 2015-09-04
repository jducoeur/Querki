package querki.links

import scala.concurrent.Future

import models.{Kind, PropertyBundle, PTypeBuilder, Wikitext}

import querki.core.URLableType
import querki.ecology._
import querki.values.{ElemValue, QLContext, SpaceState}

object MOIDs extends EcotIds(27) {
  val ChoiceOrderOID = moid(1)
}

class LinksEcot(e:Ecology) extends QuerkiEcot(e) with Links {
  import PublicMOIDs._
  import MOIDs._
  
  val Types = initRequires[querki.types.Types]
  
  def LinkValue(target:OID):QValue = ExactlyOne(LinkType(target))
      
  /***********************************************
   * TYPES
   ***********************************************/
  
  lazy val URLType = new SystemType[QURL](URLTypeOID,
    toProps(
      setName("URL Type"),
      Summary("The URL of a web page"),
      Details("""This Property holds simply a pointer to a webpage, any webpage. The syntax is currently
          |pretty forgiving, but don't count on that -- it may get stricter about requiring a well-formed URL
          |in the future, so it is only recommended that this be used with full URLs.
          |
          |This Type is useful, but not as useful as one would wish, since it doesn't allow you to give a description
          |of what this URL is linking to. That is why its name was changed from "External Link Type". A new, more
          |sophisticated External Link Type will be added in the near future.""".stripMargin)
    )) with PTypeBuilder[QURL, String] with URLableType
  {
    override def editorSpan(prop:Property[_,_]):Int = 6
  
    def doDeserialize(v:String)(implicit state:SpaceState) = QURL(v)
    def doSerialize(v:QURL)(implicit state:SpaceState) = v.url
    def doWikify(context:QLContext)(v:QURL, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = {
      val display = displayOpt.getOrElse(Wikitext(v.url))
      Future.successful(Wikitext("[") + display + Wikitext("](" + v.url + ")"))
    }
  
    def getURL(context:QLContext)(elem:ElemValue):Option[String] = {
      elem.getOpt(this).map(_.url)
    }
  
    // For the simple URL Type, the display is the same as the URL:
    def getDisplay(context:QLContext)(elem:ElemValue):Option[String] = {
      elem.getOpt(this).map(_.url)
    }
  
    def doDefault(implicit state:SpaceState) = new QURL("")
    override def wrap(raw:String):valType = new QURL(raw)
  }

  override lazy val types = Seq(
    URLType
  )
    
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
	/**
	 * Meta-property, set on Properties of LinkType, to filter what to Link to.
	 */
	lazy val LinkKindProp = new SystemProperty(LinkKindOID, IntType, QList,
	    toProps(
	      setName("Link Kind"),
	      SkillLevel(SkillLevelAdvanced),
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
	
	lazy val LinkAllowAppsProp = new SystemProperty(LinkAllowAppsOID, YesNoType, Optional,
	    toProps(
	      setName("Allow Links to Apps"),
	      Summary("Should this Property allow Links to Things in Apps?"),
	      Details("""Normally, links are only to other Things in this Space. But if this flag is set, this
	          |says that this Property should allow linking to Things in Apps of this Space.
	          |
	          |This is an advanced property, and not intended for casual use.""".stripMargin),
	      AppliesToKindProp(Kind.Property),
	      SkillLevel(SkillLevelAdvanced)
	      ))
	
	lazy val LinkModelProp = new SystemProperty(LinkModelOID, LinkType, Optional,
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
	      Types.AppliesToTypesProp(LinkType, querki.tags.MOIDs.NewTagSetOID),
	      LinkToModelsOnlyProp(true)
	      ))
	
	// TODO: As it says, replace this with a more general Link Filter property. That will need bigger
	// refactorings, though: I started to build that, only to discover that SpaceState.linkCandidates
	// doesn't have all the request-context information needed to resolve a QL Expression.
	lazy val LinkToModelsOnlyProp = new SystemProperty(LinkToModelsOnlyOID, YesNoType, ExactlyOne,
	    toProps(
	      setName("Link to Models Only"),
	      AppliesToKindProp(Kind.Property),
	      (querki.identity.skilllevel.MOIDs.SkillLevelPropOID -> ExactlyOne(LinkType(querki.identity.skilllevel.MOIDs.SkillLevelAdvancedOID))),
	      Summary("Only allow this Property to Link to Models"),
	      Details("""If set to true, this Link Property will only show Models as options to link to in the editor.
	          |
	          |This is an advanced property, and something of a hack -- don't get too comfortable with it. In the
	          |medium term, it should get replaced by a more general LinkFilter property that lets you specify which
	          |Things to link to.""".stripMargin)))
	
	lazy val NoCreateThroughLinkProp = new SystemProperty(NoCreateThroughLinkOID, YesNoType, ExactlyOne,
	    toProps(
	      setName("Is a Choice"),
	      NotInherited,
	      Core.ModelOnlyProp(true),
	      Summary("Set this to declare that this Model is a Choice, and its Instances are the options to choose from."),
	      Details("""When you create a Link Property in the Editor, you can set the "Link Model" -- the sort of Thing
	          |that this Property points to. The Editor then lets you choose from all of the existing Instances of that
	          |Model, and also lets you create a new one.
	          |
	          |Sometimes, though, you don't want to create any new ones from the Editor. In particular, if you've already
	          |created all of the Instances of this Model that you ever expect to want, then it is simply annoying to have
	          |that option. In that case, put this Property on your Model, and set it to True -- it will make that option
	          |in the Editor go away.""".stripMargin)))
  
  lazy val ChoiceOrderProp = new SystemProperty(ChoiceOrderOID, LinkType, QList,
    toProps(
      setName("Choice Order"),
      Summary("If this is set on a Choice Model, it declares the order in which the options should be listed.")))

  override lazy val props = Seq(
    LinkKindProp,
    LinkAllowAppsProp,
    LinkModelProp,
    LinkToModelsOnlyProp,
    NoCreateThroughLinkProp,
    ChoiceOrderProp
  )
}