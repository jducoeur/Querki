package querki.tags

import querki.ecology._

import models.{AsDisplayName, Collection, Kind, Thing, ThingId, UnknownOID, Wikitext}

import querki.basic.{PlainText, PlainTextBaseType}
import querki.core.{NameableType, NameTypeBasis, NameUtils, QLText, TextTypeBasis}
import querki.ql.QLPhrase
import querki.util.SafeUrl
import querki.values._

/**
 * TODO: this should probably absorb more of the concept of "tags", maybe even including the Types.
 */
class TagsEcot(e:Ecology) extends QuerkiEcot(e) with Tags with querki.core.MethodDefs with NameUtils with NameTypeBasis with TextTypeBasis with PlainTextBaseType {
  import MOIDs._
  
  val Links = initRequires[querki.links.Links]
  val Basic = initRequires[querki.basic.Basic]
  
  lazy val PlainTextType = Basic.PlainTextType
  lazy val NameType = Core.NameType
    
  lazy val LinkModelOID = querki.links.MOIDs.LinkModelOID

  /***********************************************
   * TYPES
   ***********************************************/

  lazy val TagSetType = new NameTypeBase(TagSetOID, 
      toProps(
        setName("Old Tag Set Type"),
        Basic.DeprecatedProp(true))) 
  {
    override def editorSpan(prop:Property[_,_]):Int = 12
    
    override def requiredColl:Option[Collection] = Some(Core.QSet)
    
    // TODO: this should probably get refactored with LinkType? They're different ways of
    // expressing the same concepts; it's just that Links are OID-based, whereas Names/Tags are
    // name-based.
    def doWikify(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None) = nameToLink(context)(v)
    
    override def renderProperty(prop:Property[_,_])(implicit request:RequestContext):Option[Wikitext] = {
      Some(QL.process(querki.core.QLText("""These tags are currently being used:
[[_tagsForProperty -> _sort -> _bulleted]]"""), prop.thisAsContext))
    }
  }

  lazy val NewTagSetType = new PlainTextType(NewTagSetOID, "Tag Set Type") {
    override def editorSpan(prop:Property[_,_]):Int = 12
  
    override def requiredColl = Some(Core.QSet)
 
    def equalNames(str1:PlainText, str2:PlainText):Boolean = {
      str1.text.toLowerCase.contentEquals(str2.text.toLowerCase())
    }
  
    override def doWikify(context:QLContext)(v:PlainText, displayOpt:Option[Wikitext] = None) = {
      val display = displayOpt.getOrElse(Wikitext(v.text))
      // NOTE: yes, there is danger of Javascript injection here. We deal with that at the QText layer,
      // since that danger is there in ordinary QText as well.
      Wikitext("[") + display + Wikitext(s"](${SafeUrl(v.text)})") 
    }
  
    override def doComp(context:QLContext)(left:PlainText, right:PlainText):Boolean = { left.text < right.text } 
      
    override def doMatches(left:PlainText, right:PlainText):Boolean = equalNames(left, right)
    
    override def renderProperty(prop:Property[_,_])(implicit request:RequestContext):Option[Wikitext] = {
      Some(QL.process(QLText("""These tags are currently being used:
[[_tagsForProperty -> _sort -> _bulleted]]"""), prop.thisAsContext))
    }
  }

  override lazy val types = Seq(
    TagSetType,
    NewTagSetType
  )

  /**
   * This is essentially a pseudo-Thing, produced when you navigate to an unknown Name. It basically
   * exists to support the display of the Undefined Tag View.
   */
  case class TagThing(name:String, space:SpaceState)(implicit e:Ecology) extends Thing(UnknownOID, space.id, UnknownOID, Kind.Thing, () => Thing.emptyProps, querki.time.epoch)(e) {
    override lazy val displayName = name
    override lazy val canonicalName = Some(name)
    override lazy val toThingId:ThingId = new AsDisplayName(name)
  
    override def render(implicit rc:RequestContext, prop:Option[Property[_,_]] = None):Wikitext = {
      implicit val s = space
      val model = preferredModelForTag(space, name)
      val propAndValOpt = model.getPropOpt(ShowUnknownProp) orElse space.getPropOpt(ShowUnknownProp)
      val nameVal = ExactlyOne(PlainTextType(name))
      val nameAsContext = QLContext(nameVal, Some(rc))
      // TODO: the link below shouldn't be so hard-coded!
      propAndValOpt.map(pv => pv.render(nameAsContext)).getOrElse(Wikitext(name + " doesn't exist yet. [Click here to create it.](edit?thingId=" + SafeUrl(name) + ")"))    
    }
  }
  
  def getTag(name:String, state:SpaceState):Thing = TagThing(name, state)

  def fetchTags(space:SpaceState, propIn:Property[_,_]):Set[String] = {
    implicit val s = space
    val thingsWithProp = space.thingsWithProp(propIn)
    if (propIn.pType == TagSetType) {
      val prop = propIn.confirmType(TagSetType).get
      (Set.empty[String] /: thingsWithProp) { (set, thing) =>
        set ++ thing.getProp(prop).rawList
      }
    } else if (propIn.pType == NewTagSetType) {
      val prop = propIn.confirmType(NewTagSetType).get
      (Set.empty[String] /: thingsWithProp) { (set, thing) =>
        set ++ thing.getProp(prop).rawList.map(_.text)
      }      
    } else
      // TODO: should this be a PublicException?
      throw new Exception("Trying to fetchTags on a non-Tag Property!")
  }
    
  def preferredModelForTag(implicit state:SpaceState, nameIn:String):Thing = {
    val tagProps = state.propsOfType(TagSetType).filter(_.hasProp(LinkModelOID))
    val newTagProps = state.propsOfType(NewTagSetType).filter(_.hasProp(LinkModelOID))
    val name = canonicalize(nameIn)
    val plainName = PlainText(nameIn)
    if (tagProps.isEmpty && newTagProps.isEmpty)
      state.interface[querki.basic.Basic].SimpleThing
    else {
      val candidates = state.allThings.toSeq
    
      // Find the first Tag Set property (if any) that is being used with this Tag:
      val tagPropOpt:Option[Property[_,_]] = tagProps.find { prop =>
        val definingThingOpt = candidates.find { thing =>
          val propValOpt = thing.getPropOpt(prop)
          propValOpt.map(_.contains(name)).getOrElse(false)
        }
        definingThingOpt.isDefined
      }
      
      val newTagPropOpt:Option[Property[_,_]] = newTagProps.find { prop =>
        val definingThingOpt = candidates.find { thing =>
          val propValOpt = thing.getPropOpt(prop)
          propValOpt.map(_.contains(plainName)).getOrElse(false)
        }
        definingThingOpt.isDefined
      }
      
      val definingPropOpt = newTagPropOpt orElse tagPropOpt
      
      // Get the Link Model for that property:
      val modelOpt = 
        for (
          tagProp <- definingPropOpt;
          linkModelPropVal <- tagProp.getPropOpt(Links.LinkModelProp);
          modelId <- linkModelPropVal.firstOpt;
          model <- state.anything(modelId)
          )
          yield model

      modelOpt.getOrElse(state.interface[querki.basic.Basic].SimpleThing)
    }
  }

  lazy val ShowUnknownProp = new SystemProperty(ShowUnknownOID, LargeTextType, ExactlyOne,
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

  
  /***********************************************
   * FUNCTIONS
   ***********************************************/  

  lazy val TagRefsMethod = new InternalMethod(TagRefsOID,
    toProps(
      setName("_tagRefs"),
      Summary("Produces a List of all Things that have the received Thing or Name as a Tag"),
      Details("""    NAME -> _tagRefs -> THINGS
          |_tagRefs is usually the right way to answer the question "what points to this?" For example, if I wanted to
          |show a bullet list of everything that points to the current Thing, I would simply say:
          |    _tagRefs -> _bulleted
          |_tagRefs is designed to receive a "name" (which is what a Tag is). If you send it a Thing, then it will use
          |the Name of that Thing.
          |
          |NOTE: _tagRefs and _refs are closely related concepts. They currently work differently, but we might
          |at some point combine them for simplicity.""".stripMargin)))
  { 
    override def qlApply(inv:Invocation):QValue = {
      val context = inv.context
      
      val elemT = context.value.pType
      elemT match {
        case nameable:NameableType => {
          val allProps = context.state.allProps.values
          val tagProps = allProps.filter(prop => prop.pType == TagSetType || prop.pType == NewTagSetType)
          val name = nameable.getName(context)(context.value.first)
          val thingOpt = elemT match {
            case LinkType => Core.followLink(context)
            case _ => None
          }
          val namePt = thingOpt.map(thing => PlainText(thing.unsafeDisplayName)).getOrElse(PlainText(name))
          val candidates = context.state.allThings
        
          def hasThisTag(candidate:Thing):Boolean = {
            tagProps.exists{ prop =>
              val propAndVal = candidate.localProp(prop)
              val found = prop.pType match {
                case TagSetType => {
	              val candidateTags:Option[List[String]] = propAndVal.map(_.v.rawList(TagSetType))
	              candidateTags.map(_.exists { candidateName => equalNames(candidateName, name) })
                }
                case NewTagSetType => {
	              val candidateTags:Option[List[PlainText]] = propAndVal.map(_.v.rawList(NewTagSetType))
	              candidateTags.map(_.exists { candidateName => NewTagSetType.equalNames(candidateName, namePt) })                
                }
              }
              found.getOrElse(false)
            }
          }
        
          Core.listFrom(candidates.filter(hasThisTag), Core.LinkFromThingBuilder)
        }
        case _ => WarningValue("_tagRefs can only be used with a Tag or Link, not " + elemT.displayName)
      }
    }
  }

  lazy val TagsForPropertyMethod = new InternalMethod(TagsForPropertyOID,
    toProps(
      setName("_tagsForProperty"),
      Summary("Show all the Tags that are defined for this Property"),
      // TODO: this isn't displaying properly. Why not? It looks like the "" nested inside of the indirect
      // Property is causing the problem -- I am getting a syntax error *claiming* to be in Default View,
      // pointing at the first "":
      Details("""    TAG PROPERTY._tagsForProperty -> LIST OF TAGS
          |_tagsForProperty can be used on any Property whose Type is Tag Set. It produces a list of all of the
          |tags that have been used in that Property so far.
          |
          |Typically, you then feed the results of this to _tagRefs, to get the Things that use that Tag. For example,
          |if I had a list of Wines, using a Tag Set Property giving its "Wine Color", I could say:
          |    \[[Wine Color._tagsForProperty -> \""* \____: \[[_tagRefs -> _commas\]]\""\]]
          |to produce a list like:
          |* Red: Pinot Noir, Shiraz
          |* White: Pinot Gris, Chardonnay
          |""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      applyToIncomingThing(inv.definingContext.get) { (shouldBeProp, _) =>
        shouldBeProp match {
          case prop:Property[_,_] if (prop.pType == TagSetType || prop.pType == NewTagSetType) => {
            Core.listFrom(fetchTags(inv.state, prop), TagSetType)
          }
          case _ => WarningValue("The _tagsForProperty method can only be used on Tag Set Properties")
        } 
      }    
    }
  }

  override lazy val props = Seq(
    ShowUnknownProp,
    TagRefsMethod,
    TagsForPropertyMethod
  )
}