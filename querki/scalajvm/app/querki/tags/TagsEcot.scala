package querki.tags

import scala.xml.NodeSeq

import querki.ecology._

import models.{AsDisplayName, Collection, DisplayPropVal, DisplayText, Kind, PropertyBundle, PType, Thing, ThingId, UnknownOID, Wikitext}

import querki.globals._

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
  
  lazy val PropPaths = interface[querki.types.PropPaths]
  
  lazy val PlainTextType = Basic.PlainTextType
  lazy val NameType = Core.NameType
    
  lazy val LinkModelOID = querki.links.PublicMOIDs.LinkModelOID

  /***********************************************
   * TAGREF CACHE
   * 
   * The TagRefCache records all of the Tag references -- that is, which Things use a given Tag --
   * for each Property. It is computed on-demand when we need to look up the references for that
   * Property. It is built upon the SpaceState's dynamic cache.
   ***********************************************/
  
  lazy val cacheKey = StateCacheKey(ecotId, "tagRefs")
  type TagMapT = scala.collection.mutable.HashMap[String, scala.collection.mutable.Set[Thing]] with scala.collection.mutable.MultiMap[String, Thing]
  type TagRefCacheT = scala.collection.concurrent.Map[OID, TagMapT]
  def trCache(implicit state:SpaceState) = 
    state.fetchOrCreateCache(cacheKey, { scala.collection.concurrent.TrieMap.empty[OID, TagMapT] }).asInstanceOf[TagRefCacheT]
  
  // The nameCache stores the map from canonical to actual Tag Names, as best we can figure out:
  lazy val nameCacheKey = StateCacheKey(ecotId, "tagNames")
  type NameCacheT = scala.collection.concurrent.Map[String, String]
  def nameCache(implicit state:SpaceState) =
    state.fetchOrCreateCache(nameCacheKey, { scala.collection.concurrent.TrieMap.empty[String, String] }).asInstanceOf[NameCacheT]
  
  /**
   * The heart of Tag reference caching.
   * 
   * For any given Property, this will compute *all* of the Tag references for that Property in this Space, *once*,
   * and cache the results with the Space. That way, we don't need to iterate over all the Things in the Space over and
   * over again.
   * 
   * Note that, while the Map under the hood here is technically mutable, that is solely for ease of building it. It should
   * be considered to be immutable once it is returned from this function.
   * 
   * TODO: the names in here should theoretically be canonicalized, but we *also* care about storing them in their
   * "real" form. How do we best handle this?
   */
  def cachedTagRefsFor(rawProp:Property[_,_])(implicit state:SpaceState):scala.collection.Map[String, scala.collection.mutable.Set[Thing]] = {
    // Note that the trCache is indexed by Property OID:
    trCache.getOrElseUpdate(rawProp.id, 
      {
        def addBindings[VT](prop:Property[_,_], pt:PType[VT], getTag: VT => String)(implicit state:SpaceState):TagMapT = 
        {
          val prop = rawProp.confirmType(pt).get
          val tagMap = new scala.collection.mutable.HashMap[String, scala.collection.mutable.Set[Thing]] with scala.collection.mutable.MultiMap[String, Thing]
          for {
	        path <- PropPaths.pathsToProperty(prop)(state)
	        thing <- state.allThings
	        pv <- path.getPropOpt(thing)(state)
	        text <- pv.rawList
          }
            tagMap.addBinding(canonicalize(getTag(text)), thing)
          tagMap          
        }
      
        rawProp.pType match {
          case NewTagSetType => addBindings(rawProp, NewTagSetType, { tag:PlainText => tag.text })
          case TagSetType => addBindings(rawProp, TagSetType, { tag:String => tag })
          case _ => throw new Exception(s"cachedTagRefsFor got unexpected Type ${rawProp.pType} from Property $rawProp")
        }
      }
    )
  }

  /***********************************************
   * TYPES
   ***********************************************/

  lazy val TagSetType = new NameTypeBase(TagSetOID, 
      toProps(
        setName("Old Tag Set Type"),
        Basic.DeprecatedProp(true),
        Summary("The original version of Tags"),
        Details("""This Type was originally what you would get if you created a Tag Set Property.
            |It is now deprecated, and you should no longer create Properties of this Type.
            |
            |Technically, Old Tag Sets were built on top of Name Type -- each Tag was technically a Name.
            |We discovered that, in practice, this was too limiting, since the Name Type only allows a very
            |small fraction of the possible text; things would break if you did something as simple as put
            |an apostrophe in a Tag.
            |
            |So this Type was replaced by what is now called Tag Set Type, which is built on top of Plain Text Type
            |and is much more powerful and flexible.""".stripMargin))) 
  {
    override def editorSpan(prop:Property[_,_]):Int = 12
    
    override def requiredColl:Option[Collection] = Some(Core.QSet)
    
    // TODO: this should probably get refactored with LinkType? They're different ways of
    // expressing the same concepts; it's just that Links are OID-based, whereas Names/Tags are
    // name-based.
    def doWikify(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = nameToLink(context)(v)
    
    override def renderProperty(prop:Property[_,_])(implicit request:RequestContext):Option[Wikitext] = {
      Some(QL.process(querki.core.QLText("""These tags are currently being used:
[[_tagsForProperty -> _sort -> _bulleted]]"""), prop.thisAsContext))
    }
  }

  lazy val NewTagSetType = new PlainTextType(NewTagSetOID, 
      toProps(
        setName("Tag Type"),
        Summary("A collection of arbitrary Tags that apply to this Thing"),
        Details("""A Tag Set is a way to add a bunch of "tags" to a Thing. It is typically used to
            |list the characteristics of this Thing.
            |
            |Note that a Tag is, technically just a Plain Text value, and can contain almost anything.
            |However, if it matches the Display Name of a Thing, it will generally display as a link to that
            |Thing if you simply show it, and if it doesn't match a Thing, clicking on it will allow you to
            |create a Thing by that name. This way, you can add additional details about what this Tag means.
            |
            |It is strongly recommended that you create Sets of Tags -- there is special UI support for this,
            |and we've found that Sets of Tags are one of the most useful sorts of data in Querki. But you
            |are no longer required to do so. (Originally, this was called "Tag Set Type", and could only be
            |a Set, but we decided to make it less restrictive.)
            |
            |When you select Tag Set Type, the Editor will ask you if you want to Link to a Model. This is optional,
            |but can be very useful -- if it is set, it restricts the Tags that get offered to you when you are
            |doing data entry. If this Property has any sort of meaning -- if the Tag Set isn't completely open-ended
            |and arbitrary -- consider first creating a Model (which doesn't need anything more than a Display Name),
            |and using it as the Link Model for the Property. That will help keep your Tags better-organized.""".stripMargin))) 
  {
    override def editorSpan(prop:Property[_,_]):Int = 12
    
    override def renderInputXml(prop:Property[_,_], context:QLContext, currentValue:DisplayPropVal, v:ElemValue):NodeSeq = {
      renderAnyText(prop, context, currentValue, v, this) { cv =>
        <input type="text" class="_tagInput" data-isnames="true" value={cv}/>
      }
    }
  
    override def doWikify(context:QLContext)(v:PlainText, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = {
      val display = displayOpt.getOrElse(Wikitext(v.text))
      // NOTE: yes, there is danger of Javascript injection here. We deal with that at the QText layer,
      // since that danger is there in ordinary QText as well.
      Wikitext("[") + display + Wikitext(s"](${SafeUrl(v.text)})") 
    }
    
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
  case class TagThing(nameIn:String, space:SpaceState)(implicit e:Ecology) 
    extends Thing(UnknownOID, space.id, preferredModelForTag(space, SafeUrl.decode(nameIn)), Kind.Thing, () => Thing.emptyProps, querki.time.epoch)(e)
    with IsTag
  {
    // Undo the effects of SafeUrl.
    // TODO: this arguably doesn't belong here. Should we be dealing with this up in Application instead? Are
    // real Thing names working properly with complex characters? If so, how?
    val name = SafeUrl.decode(nameIn)
    override lazy val displayName = name
    override lazy val unsafeDisplayName = name
    override def nameOrComputed(implicit rc:RequestContext) = DisplayText(name)
    override def unsafeNameOrComputed(implicit rc:RequestContext) = name
    override lazy val canonicalName = Some(name)
    override lazy val linkName = Some(name)
    override lazy val toThingId:ThingId = new AsDisplayName(name)
    
    def pseudoModel = preferredModelForTag(space, name)
      
   override def render(implicit rc:RequestContext, prop:Option[Property[_,_]] = None):Wikitext = {
      implicit val s = space
      val model = pseudoModel
      val propAndValOpt = model.getPropOpt(ShowUnknownProp) orElse space.getPropOpt(ShowUnknownProp)
      val nameVal = ExactlyOne(PlainTextType(name))
      val nameAsContext = QLContext(nameVal, Some(rc))
      // TODO: the link below shouldn't be so hard-coded!
      propAndValOpt.map(pv => pv.render(nameAsContext)).getOrElse(Wikitext(name + " doesn't exist yet. [Click here to create it.](edit?thingId=" + SafeUrl(name) + ")"))    
    }
  }
  
  def getTag(name:String, state:SpaceState):Thing = TagThing(name, state)

  def fetchTags(state:SpaceState, propIn:Property[_,_]):Set[String] = {
    implicit val s = state
    val tagList = cachedTagRefsFor(propIn).keys
    // Screen out any null tags, in case they have snuck in:
    tagList.toSet.filter(_.length > 0)
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
  
  def isTaggableProperty(prop:AnyProp)(implicit state:SpaceState):Boolean = {
    val cType = prop.cType
    val pType = prop.pType
    (cType == QSet && (pType == Core.NameType || pType == TagSetType || pType == LinkType || pType == NewTagSetType))
  }

  lazy val ShowUnknownProp = new SystemProperty(ShowUnknownOID, LargeTextType, ExactlyOne,
    toProps(
      setName("Undefined Tag View"),
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

  def hasName(pv:PropAndVal[_], name:String, plaintext:PlainText, prop:Property[_,_]):Boolean = {
    prop.pType match {
      case TagSetType => {
	    val candidateTags:List[String] = pv.v.rawList(TagSetType)
	    candidateTags.exists { candidateName => equalNames(candidateName, name) }
      }
      case NewTagSetType => {
        val candidateTags:List[PlainText] = pv.v.rawList(NewTagSetType)
        candidateTags.exists { candidateName => NewTagSetType.equalNames(candidateName, plaintext) }                
      }
    }
  }

  /**
   * TODO: man, this code is horrible. It needs a *lot* of cleanup. It is extraordinarily inefficient -- enough
   * so that it may be better for us to precalculate all of the references and stick them into the State Cache
   * instead.
   */
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
          |    NAME -> PROPERTY._tagRefs -> THINGS
          |
          |If you specify a PROPERTY like this, it will only produce Things that point to this through that
          |*specific* Tag Property.
          |
          |NOTE: _tagRefs and _refs are closely related concepts. They currently work differently, but we might
          |at some point combine them for simplicity.""".stripMargin)))
  { 
    override def qlApply(inv:Invocation):QValue = {
      implicit val s = inv.state
      for {
        dummy <- inv.preferCollection(QSet)
        nameableType <- inv.contextTypeAs[NameableType]
        definingProp = inv.definingContextAsPropertyOf(NewTagSetType).get
        tagProps =
          if (definingProp.isEmpty)
            // There was no property specified, so use all Tag Properties
            inv.state.propList.filter(prop => prop.pType == TagSetType || prop.pType == NewTagSetType)
          else
            definingProp
//        thingOpt = nameableType match {
//            case LinkType => Core.followLink(inv.context)
//            case _ => None
//          }
        prop <- inv.iter(tagProps)
        refMap = cachedTagRefsFor(prop)
        tagElem <- inv.contextElements
        name = nameableType.getName(inv.context)(tagElem.value.first)
        refs = refMap.get(name).getOrElse(Set.empty[Thing])
        candidate <- inv.iter(refs)
//        namePt = thingOpt.map(thing => PlainText(thing.unsafeDisplayName)).getOrElse(PlainText(oldName))
//        // Since the tag might be contained inside a Model Value, we need to figure out all the paths
//        // that might be used to get to the Property:
//        paths = PropPaths.pathsToProperty(prop)(inv.state)
//        candidate <- inv.iter(inv.context.state.allThings)
//        path <- inv.iter(paths)
//        propAndVal <- inv.iter(path.getPropOpt(candidate)(inv.state))
//        if (hasName(propAndVal, oldName, namePt, prop))
      }
        yield ExactlyOne(LinkType(candidate))
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
          |
          |ADVANCED: It is legal to use this with a received Property, instead of using the dot syntax. But remember
          |that it must be applied to the Property *itself*, so you might have to use _self to get the right results,
          |like this:
          |    \[[Wine Color._self -> _tagsForProperty -> \""* \____: \[[_tagRefs -> _commas\]]\""\]]
          |In general, you only want to receive the Property like this if you are passing it into a Function.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      applyToIncomingThing(inv.preferDefiningContext) { (shouldBeProp, _) =>
        shouldBeProp match {
          case prop:Property[_,_] if (prop.pType == TagSetType || prop.pType == NewTagSetType) => {
            prop.pType match {
              case TagSetType => Core.listFrom(fetchTags(inv.state, prop), TagSetType)
              case NewTagSetType => Core.listFrom(fetchTags(inv.state, prop), NewTagSetType)
            }
          }
          case _ => WarningValue("The _tagsForProperty method can only be used on Tag Set Properties")
        } 
      }    
    }
  }
  
  lazy val resolveTagsMethod = new InternalMethod(ResolveTagsOID,
    toProps(
      setName("_resolveTags"),
      Summary("Turns any of the received Tags that name actual Things into Links to those Things"),
      Details("""    TAGS -> _resolveTags -> LINKS
          |A Tag is essentially a name -- it may or may not be the name of an actual Thing.
          |Occasionally, you want to be able to get to those Things -- for example, you might want
          |to use one of the Properties of the Thing if it exists. This function helps you do that.
          |
          |For each Tag that is received by _resolveTags, the system checks if it names a Thing. If
          |so, it produces a Link to that Thing; if not, it ignores the Tag.
          |
          |Like most Querki Functions, this works equally well with a List of Tags, a Set of Tags,
          |or a single passed-in Tag.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QValue = {
      for {
        tag <- inv.contextAllAs(NewTagSetType)
        thing <- inv.opt(inv.state.anythingByName(tag.text))
      }
        yield ExactlyOne(Core.LinkType(thing.id))
    }
  }

  override lazy val props = Seq(
    ShowUnknownProp,
    TagRefsMethod,
    TagsForPropertyMethod,
    resolveTagsMethod
  )
}