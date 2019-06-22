package querki.tags

import scala.xml.NodeSeq
import models.{Thing, UnknownOID, AsDisplayName, OID, ThingId, Collection, DisplayPropVal, DisplayText, PropertyBundle, Kind, ThingOps, PType, Property, Wikitext}
import querki.api.commonName
import querki.basic.{PlainTextBaseType, PlainText}
import querki.core._
import querki.ecology._
import querki.globals._
import querki.ql.QLPhrase
import querki.util.{HtmlEscape, SafeUrl}
import querki.values.{QValue, _}

/**
 * TODO: this should probably absorb more of the concept of "tags", maybe even including the Types.
 */
class TagsEcot(e:Ecology) extends QuerkiEcot(e) with Tags with querki.core.MethodDefs 
  with querki.core.WithQL with NameUtils with NameTypeBasis with TextTypeBasis with PlainTextBaseType 
{
  import MOIDs._
  
  val Links = initRequires[querki.links.Links]
  val Basic = initRequires[querki.basic.Basic]
  val Editor = initRequires[querki.editing.Editor]
  
  lazy val PropPaths = interface[querki.types.PropPaths]
  lazy val QL = interface[querki.ql.QL]
  lazy val TimeProvider = interface[querki.time.TimeProvider]
  
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
  // Given a canonical name from the trCache, fetch the name to show to the users:
  def publicTag(canon:String)(implicit state:SpaceState):String = nameCache(state)(canon)
  
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
          val nc = nameCache
          val prop = rawProp.confirmType(pt).get
          val tagMap = new scala.collection.mutable.HashMap[String, scala.collection.mutable.Set[Thing]] with scala.collection.mutable.MultiMap[String, Thing]
          for {
  	        path <- PropPaths.pathsToProperty(prop)(state)
  	        thing <- state.everything
  	        pv <- path.getPropOpt(thing)(state)
  	        text <- pv.rawList
  	        raw = getTag(text)
  	        canon = canonicalize(raw)
          }
          {
            // Record the actual backtag:
            tagMap.addBinding(canon, thing)
            
            // Now, record the mapping from the canonical name to the real name of this 
            nc.getOrElseUpdate(canon, {
              // There are multiple Tags that can result in a given canonical name. Check whether
              // this Tag is reified, and use that as the official name if so; otherwise, just trust
              // the Tag:
              val realName = 
                for {
                  t <- state.anythingByName(canon)
                  real <- t.linkName
                  if (real != raw)
                }
                  yield real
              realName.getOrElse(raw)
            })
          }
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
  
  val TagsTag = "Tags"

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
    def doWikify(context:QLContext)(v:String, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = 
      Future.successful(nameToLink(context)(v))
    
    override def renderProperty(prop:Property[_,_])(implicit request:RequestContext, state:SpaceState):Option[Future[Wikitext]] = {
      Some(QL.process(querki.core.QLText("""These tags are currently being used:
        |[[_tagsForProperty -> _sort -> _bulleted]]""".stripMargin), prop.thisAsContext))
    }
  }

  // Deprecated: the Type proper has moved back to Core, so that we can use it in Core Properties:
  lazy val NewTagSetType = Core.TagType

  override lazy val types = Seq(
    TagSetType
  )

  /**
    * Takes a name that we get from the front end, and figures out its "real" name.
    *
    * This is rather horribly ad-hoc, and needs re-examination, particularly in light of anywhere else that might
    * be using SafeUrl.decode(). The big issue that we sometimes need to deal with dashes, particularly in links
    * from QL.
    */
  def decodeTagName(nameIn: String): String = SafeUrl.decode(nameIn).replaceAll("-", " ")

  /**
    * Return an Exception for the case where we try to fetch a Property from a Tag.
    *
    * TODO: the existence of this function strongly indicates that our factoring sucks, and should be improved.
    */
  def tagPropError(tagName: String, propName: String): PublicException = new PublicException("Tags.noProp", tagName, propName)

  /**
   * This is essentially a pseudo-Thing, produced when you navigate to an unknown Name. It basically
   * exists to support the display of the Undefined Tag View.
   */
  case class TagThing(nameIn:String, space:SpaceState)
    extends 
      Thing(
        UnknownOID, 
        space.id, 
        preferredModelForTag(space, decodeTagName(nameIn)),
        Kind.Thing, 
        toProps(
          Basic.DisplayNameProp(HtmlEscape.escapeAll(SafeUrl.decode(nameIn)))
        ),
        querki.time.epoch)
    with IsTag
  {
    // Undo the effects of SafeUrl. Also, cope with dashes in names the way that real Things do.
    // TODO: this arguably doesn't belong here. Should we be dealing with this up in Application instead? Are
    // real Thing names working properly with complex characters? If so, how?
    val name = decodeTagName(nameIn)
    override lazy val displayName = name
    override lazy val unsafeDisplayName = name
    override lazy val canonicalName = linkName
    override lazy val linkName = Some(SafeUrl(name))
    override lazy val toThingId:ThingId = new AsDisplayName(name)

    override def thingOps(e:Ecology) = new TagThingOps(this)
  }
  
  class TagThingOps(tag:TagThing) extends ThingOps(tag)(ecology) {
    def space = tag.space
    def name = tag.name
    def pseudoModel = preferredModelForTag(space, name)
    
    override def thisAsQValue:QValue = Core.ExactlyOne(NewTagSetType(tag.name))
    
    override def nameOrComputedWiki(implicit request:RequestContext, state:SpaceState):Future[Wikitext] = Future.successful(Wikitext(name))
    override def nameOrComputed(implicit rc:RequestContext, state:SpaceState) = Future.successful(DisplayText(name))
    override def unsafeNameOrComputed(implicit rc:RequestContext, state:SpaceState) = Future.successful(name)
    
    override def render(implicit rc:RequestContext, state:SpaceState, prop:Option[Property[_,_]] = None):Future[Wikitext] = {
      val model = pseudoModel
      val propAndValOpt = model.getPropOpt(ShowUnknownProp) orElse space.getPropOpt(ShowUnknownProp)
      val nameVal = ExactlyOne(PlainTextType(name))
      val nameAsContext = QLContext(nameVal, Some(rc), TimeProvider.qlEndTime)
      // TODO: the link below shouldn't be so hard-coded!
      propAndValOpt.map(pv => pv.render(nameAsContext)).
        getOrElse(Future.successful(Wikitext(name + " doesn't exist yet. [Click here to create it.](edit?thingId=" + SafeUrl(name) + ")")))
    }
  }
  
  def getTag(name:String, state:SpaceState):Thing = TagThing(name, state)

  def fetchTags(state:SpaceState, propIn:Property[_,_]):Set[String] = {
    implicit val s = state
    val tagList = cachedTagRefsFor(propIn).keys
    // Screen out any null tags, in case they have snuck in, and translate the "canonical" names to "public" ones:
    tagList.toSet.filter(_.length > 0).map(publicTag(_))
  }
  
  // TBD: do we want to cache this Set as well, to optimize tagExists()? It's not obvious,
  // since the underlying fetchTags() does cache the per-Property values.
  def fetchAllTags(state:SpaceState):Set[String] = {
    state
      .allProps
      .values
      .filter(prop => prop.pType == NewTagSetType || prop.pType == TagSetType)
      .map(prop => fetchTags(state, prop))
      .toSet
      .flatten
  }
  
  def tagExists(name:String, state:SpaceState):Boolean = {
    fetchAllTags(state).contains(name)
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
    (cType == QSet && 
        (pType == Core.NameType || 
         pType == TagSetType || 
         pType.isInstanceOf[querki.core.IsLinkType] || 
         pType == NewTagSetType))
  }
  
  def getUndefinedTagView(modelId:OID)(implicit state:SpaceState):QLText = {
    val modelUndefOpt = state.anything(modelId) flatMap { model =>
      model.getPropOpt(ShowUnknownProp)
    }
    
    val undef = (modelUndefOpt orElse state.getPropOpt(ShowUnknownProp)) flatMap (_.firstOpt)
    
    undef.getOrElse(QLText(querki.tags.defaultDisplayText))
  }

  lazy val ShowUnknownProp = new SystemProperty(ShowUnknownOID, LargeTextType, ExactlyOne,
    toProps(
      setName("Undefined Tag View"),
      Categories(TagsTag),
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
          |will use that View. (Technically, this means all Tags that are used in a Tag Set whose `Restrict to Model`
          |points to this Model.)
          |
          |There is a simple default value that is defined on every Space by default. But you should feel free
          |to override that to do something more interesting, especially if you are doing interesting things
          |with Tags in your Space.""".stripMargin)))
  
  lazy val IsReifiedTagProp = new SystemProperty(IsReifiedTagOID, YesNoType, Optional,
    toProps(
      setName(commonName(_.tags.isReifiedTag)),
      Core.InternalProp(true),
      Editor.NotEditableProp(true),
      Categories(TagsTag),
      Summary("Set automatically when you turn a Tag into a real Thing"),
      Details("""You should usually not need to worry about this. This Property mainly controls the way the
        |reified tag displays, if its Model doesn't have a Default View.""".stripMargin)))

  
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
   * Note that this is called from both _tagRefs and _refs.
   */
  def tagRefs(inv:Invocation):QFut = {
    implicit val s = inv.state
    for {
      dummy <- inv.preferCollection(QSet)
      nameableType <- inv.contextTypeAs[NameableType]
      definingProp <- inv.definingContextAsOptionalPropertyOf(NewTagSetType)
      tagProps =
        definingProp match {
          case Some(p) => Iterable(p)
          // There was no property specified, so use all Tag Properties
          case None => inv.state.propList.filter(prop => prop.pType == TagSetType || prop.pType == NewTagSetType)
        }
      prop <- inv.iter(tagProps)
      refMap = cachedTagRefsFor(prop)
      tagElem <- inv.contextElements
      name <- inv.iter(nameableType.getNames(inv.context)(tagElem.value.first))
      refs = refMap.get(name).getOrElse(Set.empty[Thing])
      candidate <- inv.iter(refs)
    }
      yield ExactlyOne(LinkType(candidate))
  }

  lazy val TagRefsMethod = new InternalMethod(TagRefsOID,
    toProps(
      setName("_tagRefs"),
      Categories(TagsTag),
      Summary("Produces a List of all Things that have the received Thing or Name as a Tag"),
      Details("""```
          |NAME -> _tagRefs -> THINGS
          |```
          |_tagRefs is usually the right way to answer the question "what points to this?" For example, if I wanted to
          |show a bullet list of everything that points to the current Thing, I would simply say:
          |```
          |_tagRefs -> _bulleted
          |```
          |_tagRefs is designed to receive a "name" (which is what a Tag is). If you send it a Thing, then it will use
          |the Name of that Thing.
          |```
          |NAME -> PROPERTY._tagRefs -> THINGS
          |```
          |
          |If you specify a PROPERTY like this, it will only produce Things that point to this through that
          |*specific* Tag Property.
          |
          |**Note:** use '_tagRefs' only if you specifically have a Tag Property. If you have a Thing, and want to get
          |*all* of the references to it, both through Tag and Thing Properties, use `_allRefs` instead.""".stripMargin)))
  { 
    override def qlApply(inv:Invocation):QFut = {
      tagRefs(inv)
    }
  }

  lazy val TagsForPropertyMethod = new InternalMethod(TagsForPropertyOID,
    toProps(
      setName("_tagsForProperty"),
      Categories(TagsTag),
      Summary("Show all the Tags that are defined for this Property"),
      Signature(
        expected = Some(Seq(LinkType), "A Tag Property (if it is not in the defining context)"),
        reqs = Seq.empty,
        opts = Seq.empty,
        returns = (LinkType, "All of the tags used for that Property in that Space."),
        defining = Some(false, Seq(LinkType), "A Tag Property")
      ),
      Details("""_tagsForProperty can be used on any Property whose Type is Tag Set. It produces a list of all of the
          |tags that have been used in that Property so far.
          |
          |Typically, you then feed the results of this to _tagRefs, to get the Things that use that Tag. For example,
          |if I had a list of Wines, using a Tag Set Property giving its "Wine Color", I could say:
          |```
          |\[[Wine Color._tagsForProperty -> \""* \____: \[[_tagRefs -> _commas\]]\""\]]
          |```
          |to produce a list like:
          |* Red: Pinot Noir, Shiraz
          |* White: Pinot Gris, Chardonnay
          |
          |ADVANCED: It is legal to use this with a received Property, instead of using the dot syntax. But remember
          |that it must be applied to the Property *itself*, so you might have to use _self to get the right results,
          |like this:
          |```
          |\[[Wine Color._self -> _tagsForProperty -> \""* \____: \[[_tagRefs -> _commas\]]\""\]]
          |```
          |In general, you only want to receive the Property like this if you are passing it into a Function.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        (shouldBeProp, _) <- inv.preferDefiningContext.bundlesAndContextsForProp(this)
      }
        yield {
          shouldBeProp match {
            case prop:Property[_,_] if (prop.pType == TagSetType || prop.pType == NewTagSetType) => {
              prop.pType match {
                case TagSetType => Core.listFrom(fetchTags(inv.state, prop), TagSetType)
                case NewTagSetType => Core.listFrom(fetchTags(inv.state, prop), NewTagSetType)
              }
            }
            case _ => QL.WarningValue("The _tagsForProperty method can only be used on Tag Set Properties")
          } 
        } 
    }
  }
  
  lazy val resolveTagsMethod = new InternalMethod(ResolveTagsOID,
    toProps(
      setName("_resolveTags"),
      Categories(TagsTag),
      Summary("Turns any of the received Tags that name actual Things into Links to those Things"),
      Details("""```
          |TAGS -> _resolveTags -> LINKS
          |```
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
    override def qlApply(inv:Invocation):QFut = {
      for {
        tag <- inv.contextAllAs(NewTagSetType)
        thing <- inv.opt(inv.state.anythingByName(tag.text))
      }
        yield ExactlyOne(Core.LinkType(thing.id))
    }
  }

  override lazy val props = Seq(
    ShowUnknownProp,
    IsReifiedTagProp,
    TagRefsMethod,
    TagsForPropertyMethod,
    resolveTagsMethod
  )
}