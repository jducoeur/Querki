package querki.links

import scala.concurrent.Future

import org.querki.requester._

import models.{DisplayPropVal, Kind, PType, PTypeBuilder, PropertyBundle, Wikitext}

import querki.api.commonName
import querki.core.{LinkCandidateProvider, URLableType}
import querki.ecology._
import querki.globals._
import querki.spaces.{TCRReq, ThingChangeRequest}
import querki.spaces.messages.{CreateThing, ThingFound}
import querki.values.{ElemValue, QLContext, SpaceState}
import querki.util.{Contributor, PublicException, Publisher, SafeUrl}

object MOIDs extends EcotIds(27) {
  val ChoiceOrderOID = moid(1)
  val ChoiceTypeOID = moid(2)
  val ChoiceModelOID = moid(3)
}

class LinksEcot(e: Ecology) extends QuerkiEcot(e) with Links with querki.core.NameUtils with querki.core.LinkUtils {
  import PublicMOIDs._
  import MOIDs._

  val SpaceChangeManager = initRequires[querki.spaces.SpaceChangeManager]
  val Types = initRequires[querki.types.Types]

  lazy val Links = this // Needed for LinkUtils

  lazy val InternalProp = Core.InternalProp

  lazy val urlBase = Config.getString("querki.app.urlRoot")

  override def init = {
    SpaceChangeManager.thingChanges += ChoiceCreator
  }

  override def term = {
    SpaceChangeManager.thingChanges += ChoiceCreator
  }

  def LinkValue(target: OID): QValue = ExactlyOne(LinkType(target))

  def thingUrl(thingId: OID)(state: SpaceState): String = {
    urlBase + "u/" + state.ownerHandle + "/" + SafeUrl.apply(state.name) + "/" + thingId.toThingId
  }

  /**
   * This hooks into the Thing-change pipeline, specifically listening for the creation of Choice Properties.
   * When it sees one happening, it creates the associated Choice Model.
   *
   * TODO: this is probably entirely obsolete, since this Choice mechanism didn't come about.
   */
  private object ChoiceCreator extends Contributor[TCRReq, TCRReq] {

    // This is called whenever we get a Create or Modify request; we only care about a few
    def notify(
      evtReq: TCRReq,
      sender: Publisher[TCRReq, TCRReq]
    ): TCRReq = {
      evtReq.flatMap {
        // Iff there is no Thing (so this is a Create) *and* it's a Property...
        case tcr @ ThingChangeRequest(who, req, state, router, modelIdOpt, None, Kind.Property, props, changedProps)
            // ... *and* its PType is Choice...
            if (props.get(querki.core.MOIDs.TypePropOID).map { v => v.contains(LinkType, ChoiceTypeOID) }.getOrElse(
              false
            )) => {
          import req.RequestableActorRef

          // ... then we need to create a matching Model
          // We expect Properties to have Names. Note that this will throw an exception if we don't have one!
          val propName = props.get(Core.NameProp).get.firstAs(Core.NameType).get
          val modelName = s"_$propName Model"
          val modelCreateReq =
            CreateThing(
              who,
              state,
              Kind.Thing,
              ChoiceModelOID,
              toProps(
                setName(modelName),
                Core.IsModelProp(true)
              )
            )
          (router ? modelCreateReq).map { case ThingFound(modelId, _) =>
            val newProps = props + (LinkModelProp(modelId))
            val newChangedProps = changedProps :+ LinkModelProp.id
            ThingChangeRequest(who, req, state, router, modelIdOpt, None, Kind.Property, newProps, newChangedProps)
          }
        }

        // Otherwise, just pass the same value along:
        case tcr => RequestM.successful(tcr)
      }
    }
  }

  /**
   * *********************************************
   * THINGS
   * *********************************************
   */

  lazy val ChoiceBaseModel = new ThingState(
    ChoiceModelOID,
    systemOID,
    querki.basic.MOIDs.SimpleThingOID,
    toProps(
      setName("Multiple Choice Options"),
      Categories(LinksTag),
      Summary("This is simply the base Model for all Multiple Choice Models. (Coming soon.)"),
      Core.IsModelProp(true),
      NoCreateThroughLinkProp(true)
    )
  )

  override lazy val things = Seq(
    ChoiceBaseModel
  )

  /**
   * *********************************************
   * TYPES
   * *********************************************
   */

  lazy val URLType = new SystemType[QURL](
    URLTypeOID,
    toProps(
      setName("URL Type"),
      Categories(LinksTag),
      Summary("The URL of a web page"),
      Details(
        """This Property holds simply a pointer to a webpage, any webpage. The syntax is currently
          |pretty forgiving, but don't count on that -- it may get stricter about requiring a well-formed URL
          |in the future, so it is only recommended that this be used with full URLs.
          |
          |This Type is useful, but not as much as one might wish, since it doesn't allow you to give a description
          |of what this URL is linking to. In most cases, you will probably find the [[External Link Type]] more
          |useful.""".stripMargin
      )
    )
  ) with PTypeBuilder[QURL, String] with URLableType {
    override def editorSpan(prop: Property[_, _]): Int = 6

    def doDeserialize(v: String)(implicit state: SpaceState) = QURL(v)
    def doSerialize(v: QURL)(implicit state: SpaceState) = v.url

    def doWikify(
      context: QLContext
    )(
      v: QURL,
      displayOpt: Option[Wikitext] = None,
      lexicalThing: Option[PropertyBundle] = None
    ) = {
      val display = displayOpt.getOrElse(Wikitext(v.url))
      Future.successful(Wikitext("[") + display + Wikitext("](" + v.url + ")"))
    }

    def getURL(context: QLContext)(elem: ElemValue): Option[String] = {
      elem.getOpt(this).map(_.url)
    }

    // For the simple URL Type, the display is the same as the URL:
    def getDisplay(context: QLContext)(elem: ElemValue): Future[Option[String]] = {
      fut(elem.getOpt(this).map(_.url))
    }

    def doDefault(implicit state: SpaceState) = new QURL("")
    override def wrap(raw: String): valType = new QURL(raw)
    def doComputeMemSize(v: QURL): Int = v.url.length
  }

  lazy val ChoiceType = new LinkTypeBase(
    ChoiceTypeOID,
    toProps(
      setName("Multiple Choice"),
      // TODO: this is marked Internal until it is ready for prime time:
      setInternal,
      Categories(LinksTag),
      Summary("A choice between several specific options"),
      Details("""Sometimes, you want to be able to specify a few pre-defined values to choose between
                |in a Property. For example, in a comic-book Space, you might have a Condition Property that let
                |you choose "Excellent", "Good", "Fair" and "Poor". Or a Baseball Team Space might have a Position
                |Property of "Pitcher", "Catcher", "First Baseman" and so on.
                |
                |Use Multiple Choice to define a Property of this sort. After you create it, you will be able to
                |define the values that you can put into the Property, and choose the order in which to list them.
                |
                |Multiple Choice is intended for relatively small lists of options. While there is not a firm limit,
                |it is generally recommended for lists of ten or fewer choices.
                |
                |If the possible options are not clearly defined in advance, we recommend using Tag instead: it is
                |more flexible, and lets you add more possibilities as you add your data.
                |
                |**Advanced:** each Multiple Choice Property defines a Model, and an Instance is created for each
                |option. By default, the Model and Instances just contain Names, but you can add more Properties
                |to them, and pass them around and process them like any other Thing. This allows you to define
                |arbitrary metadata for each option, which Querki power users can use to write more powerful
                |expressions.""".stripMargin)
    )
  ) {

    override def canCoerceTo(other: PType[_]): Boolean = {
      // Note that Multiple Choice can be turned into Link, but *not* vice-versa. LinkType is
      // effectively a base class.
      other == LinkType
    }

    override def coerceTo(
      other: PType[_],
      elem: ElemValue
    ): ElemValue = {
      if (other == LinkType)
        ElemValue(elem.elem, other)
      else
        super.coerceTo(other, elem)
    }
  }

  override lazy val types = Seq(
    URLType,
    ChoiceType
  )

  /**
   * *********************************************
   * PROPERTIES
   * *********************************************
   */

  /**
   * Meta-property, set on Properties of LinkType, to filter what to Link to.
   */
  lazy val LinkKindProp = new SystemProperty(
    LinkKindOID,
    IntType,
    QList,
    toProps(
      setName("Link Kind"),
      SkillLevel(SkillLevelAdvanced),
      Categories(LinksTag),
      Summary("The Kind that this Property can contain"),
      Details("""When you create a Thing Property, if you do *not* set the `Restrict to Model` Property on it,
                |you may want to at least specify which *Kind* of Thing this can contain. There are five Kinds of
                |Things in Querki:
                |
                |* Ordinary Thing
                |* Property
                |* Space
                |* Type
                |* Collection
                |
                |99% of the time, you will want to contain ordinary Things. (And most of those times, you should set
                |Restrict to Model.) Occasionally, for very complex systems, you may want to contain a Property
                |or Space instead. You are not likely to ever want Type or Collection, but it is possible to do so.
                |
                |This is an extremely advanced property, and not intended for casual use.""".stripMargin),
      AppliesToKindProp(Kind.Property)
    )
  )

  lazy val LinkAllowAppsProp = new SystemProperty(
    LinkAllowAppsOID,
    YesNoType,
    Optional,
    toProps(
      setName("Allow Links to Apps"),
      Categories(LinksTag),
      Summary("Should this Property allow Links to Things in Apps?"),
      Details("""Normally, links are only to other Things in this Space. But if this flag is set, this
                |says that this Property should allow linking to Things in Apps of this Space.
                |
                |This is an advanced property, and not intended for casual use.""".stripMargin),
      AppliesToKindProp(Kind.Property),
      SkillLevel(SkillLevelAdvanced)
    )
  )

  lazy val LinkModelProp = new SystemProperty(
    LinkModelOID,
    LinkType,
    Optional,
    toProps(
      setName(commonName(_.links.linkModelProp)),
      Categories(LinksTag),
      Summary("Which Things can this Property contain?"),
      Details(
        """By default, Link Properties can contain *anything*. This usually isn't what
          |you want, though -- most often, you're looking for Instances of a specific Model. For example,
          |if you specify the Stylesheet Property, you only want to give Stylesheets as options:
          |it would be meaningless to have the Stylesheet contain something like a recipe or a to-do list.
          |
          |So this is a meta-Property: when you create a Thing Property, you can add this to say
          |exactly *which* Things it can contain. It is strongly recommended that you set this on all Thing Properties
          |you create -- it makes them easier to use, and tends to prevent confusing errors.
          |
          |Note that this is only enforced loosely, and you can't absolutely count upon this restriction
          |always being true. But used properly, it will steer folks in the right direction.""".stripMargin
      ),
      AppliesToKindProp(Kind.Property),
      Types.AppliesToTypesProp(LinkType, querki.tags.MOIDs.NewTagSetOID),
      LinkToModelsOnlyProp(true)
    )
  )

  // TODO: As it says, replace this with a more general Link Filter property. That will need bigger
  // refactorings, though: I started to build that, only to discover that SpaceState.linkCandidates
  // doesn't have all the request-context information needed to resolve a QL Expression.
  lazy val LinkToModelsOnlyProp = new SystemProperty(
    LinkToModelsOnlyOID,
    YesNoType,
    ExactlyOne,
    toProps(
      setName("Link to Models Only"),
      AppliesToKindProp(Kind.Property),
      (querki.identity.skilllevel.MOIDs.SkillLevelPropOID -> ExactlyOne(
        LinkType(querki.identity.skilllevel.MOIDs.SkillLevelAdvancedOID)
      )),
      Categories(LinksTag),
      Summary("Only allow this Property to Link to Models"),
      Details("""If set to true, this Link Property will only show Models as options to link to in the editor.
                |
                |This is an advanced property, and something of a hack -- don't get too comfortable with it. In the
                |medium term, it should get replaced by a more general LinkFilter property that lets you specify which
                |Things to link to.""".stripMargin)
    )
  )

  lazy val NoCreateThroughLinkProp = new SystemProperty(
    NoCreateThroughLinkOID,
    YesNoType,
    ExactlyOne,
    toProps(
      setName("Is a Choice"),
      NotInherited,
      Core.ModelOnlyProp(true),
      Categories(LinksTag),
      Summary("Set this to declare that this Model is a Choice, and its Instances are the options to choose from."),
      Details("""If you set this to True on a Model, it means that this Model is special -- its Instances are an
                |exclusive list of options to choose from. In this case, the Editor will know not to offer to create new
                |Instances when you are choosing one.
                |
                |In the near future, this will become automatic with the new Multiple Choice Type, and you
                |won't need to use it manually.""".stripMargin)
    )
  )

  lazy val ChoiceOrderProp = new SystemProperty(
    ChoiceOrderOID,
    LinkType,
    QList,
    toProps(
      setName("Choice Order"),
      Categories(LinksTag),
      Summary("If this is set on a Choice Model, it declares the order in which the options should be listed."),
      Types.AppliesToTypesProp(ChoiceType)
    )
  ) with LinkCandidateProvider {

    def getLinkCandidates(
      state: SpaceState,
      currentValue: DisplayPropVal
    ): Seq[Thing] = {
      val resultOpt = for {
        bundle <- currentValue.on
        model <- bundle.asThing
        if (model.isModel(state))
      } yield state.descendants(model.id, false, true)

      resultOpt.map(_.toSeq).getOrElse(Seq.empty)
    }
  }

  override lazy val props = Seq(
    LinkKindProp,
    LinkAllowAppsProp,
    LinkModelProp,
    LinkToModelsOnlyProp,
    NoCreateThroughLinkProp,
    ChoiceOrderProp
  )
}
