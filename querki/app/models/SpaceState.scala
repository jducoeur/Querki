package models

import language.existentials
import system._

import play.api.Logger

import OIDs._

import Thing._

import identity.User

/**
 * A Space is the Querki equivalent of a database -- a collection of related Things,
 * Properties and Types.
 * 
 * Note that, just like everything else, a Space is a special sort of Thing. It can
 * have Properties (including user-defined ones), and can potentially inherit from a
 * Model.
 * 
 * A SpaceState is a Space at a specific point in time. Operations are usually performed
 * on a SpaceState, to keep them consistent. Changes are sent to the Space, which generates
 * a new SpaceState from them.
 * 
 * TODO: implement Space inheritance -- that is, Apps.
 */
case class SpaceState(
    s:OID, 
    m:OID,
    pf:PropFetcher,
    owner:OID,
    name:String,
    // TODO: in principle, this is a List[SpaceState] -- there can be multiple ancestors:
    app:Option[SpaceState],
    types:Map[OID, PType[_]],
    spaceProps:Map[OID, Property[_,_]],
    things:Map[OID, ThingState],
    colls:Map[OID, Collection]) 
  extends Thing(s, s, m, Kind.Space, pf) 
{
  // Walks up the App tree, looking for the specified Thing of the implied type:
  // IMPORTANT: note that the OID and ThingId versions of these methods are inconsistent in their
  // return signatures! That is a historical accident.
  // TODO: these OID methods really should return Option[T]. Throwing an exception this deep in the stack
  // is tending to produce pretty damned cryptic exceptions. Instead, just make the higher levels cope
  // with the Nones.
  def resolve[T <: Thing](tid:OID)(lookup: (SpaceState) => Map[OID, T]):T = {
    lookup(this).get(tid).getOrElse(
          app.map(_.resolve(tid)(lookup)).getOrElse(throw new Exception("Couldn't find " + tid)))
  }
  def typ(ptr:OID) = resolve(ptr) (_.types)
  def prop(ptr:OID) = resolve(ptr) (_.spaceProps)
  def thing(ptr:OID) = resolve(ptr) (_.things)
  def coll(ptr:OID) = resolve(ptr) (_.colls)
  
  def resolve[T <: Thing](tid:ThingId)(lookup: (SpaceState) => Map[OID, T]):Option[T] = {
    val map = lookup(this)
    tid match {
      case AsOID(id) => map.get(id).orElse(app.flatMap(_.resolve(tid)(lookup)))
      case AsName(name) => thingWithName(name, map).orElse(app.flatMap(_.resolve(tid)(lookup)))
    }
  }
  def typ(ptr:ThingId) = resolve(ptr) (_.types)
  def prop(ptr:ThingId) = resolve(ptr) (_.spaceProps)
  def thing(ptr:ThingId) = resolve(ptr) (_.things)
  def coll(ptr:ThingId) = resolve(ptr) (_.colls)
  
  def anything(oid:OID):Option[Thing] = {
    // TODO: this should do something more sensible if the OID isn't found at all:
    things.get(oid).orElse(
      spaceProps.get(oid).orElse(
        types.get(oid).orElse(
          colls.get(oid).orElse(
            selfByOID(oid).orElse(
              app.flatMap(_.anything(oid)))))))
  }
  
  def thingWithName[T <: Thing](name:String, things:Map[OID, T]):Option[T] = {
    things.values.find { thing =>
      val thingNameOpt = thing.canonicalName
      thingNameOpt.isDefined && NameType.equalNames(thingNameOpt.get, name)
    }
  }
  
  def selfByOID(oid:OID):Option[Thing] = {
    if (oid == id) Some(this) else None
  }
  def spaceByName(tryName:String):Option[Thing] = {
    if (tryName == NameType.toInternal(name)) Some(this) else None
  }
  
  // TBD: should this try recognizing Display Names as well? I've confused myself that way once
  // or twice.
  // TBD: changed this to look up the app stack. That's clearly right sometimes, like in QL.
  // Is it always right?
  def anythingByName(rawName:String):Option[Thing] = {
    val name = NameType.toInternal(rawName)
    thingWithName(name, things).orElse(
      thingWithName(name, spaceProps).orElse(
        thingWithName(name, types).orElse(
          thingWithName(name, colls).orElse(
            spaceByName(name).orElse(
              app.flatMap(_.anythingByName(rawName)))))))
  }
  
  def anything(thingId:ThingId):Option[Thing] = {
    thingId match {
      case AsOID(oid) => anything(oid)
      case AsName(name) => anythingByName(name)
    }
  }
  
  /**
   * Returns true iff this Space has any thing with the given ID.
   */
  def contains(id:OID):Boolean = {
    things.contains(id) ||
    spaceProps.contains(id) ||
    types.contains(id) ||
    colls.contains(id)
  }
  
  /**
   * Returns all of the conventional Things.
   * 
   * This is used mainly by _refs() so far.
   * 
   * TBD: this currently does not return Props, Types or Collections; it also does not search up the
   * App tree. Should it?
   */
  def allThings:Iterable[Thing] = things.values
  
  def everythingLocal:Iterable[Thing] = things.values ++ spaceProps.values ++ types.values ++ colls.values
  
  def allProps:Map[OID, Property[_,_]] = if (app.isEmpty) spaceProps else spaceProps ++ app.get.allProps
  
  def allModels:Iterable[ThingState] = {
    implicit val s = this
    val myModels = things.values.filter(_.first(IsModelProp))
    if (app.isEmpty) {
      myModels
    } else {
      myModels ++ app.get.allModels
    }
  }
  
  def root(t:Thing):OID = {
    val modelId = t.model
    if (contains(modelId))
      root(t.getModel(this))
    else
      modelId
  }
  
  /**
   * Returns the set of external "roots" of the Things in this Space. Note that this list is composed entirely
   * of Things *not* in this Space -- it is the ones we are inheriting from. 
   */
  def thingRoots:Iterable[OID] = {
    (Set.empty[OID] /: allThings) ((set, t) => set + root(t))
  }
  
  def thingsWithProp(prop:Property[_,_]):Iterable[Thing] = {
    everythingLocal.filter(_.hasProp(prop.id)(this))
  }
  
  def propsOfType[VT](pt:PType[VT]):Iterable[Property[VT,_]] = {
    spaceProps.values.filter(_.pType == pt).map(_.confirmType(pt))
  }
  
  /**
   * Returns all of the immediate children of this Thing.
   * 
   * TBD: for the moment, this only works for Things. It probably should work on other Kinds as well?
   */
  def children(t:Thing):Iterable[Thing] = {
    val tid = t.id
    things.values.filter(_.model == tid)
  }
  
  def descendantsTyped[T <: Thing](root:OID, includeModels:Boolean, includeInstances:Boolean, map:Map[OID, T]):Iterable[Thing] = {
    map.values.filter(_.isAncestor(root)(this))
  }
  
  // TODO: this is pretty inefficient -- it is going to fully walk the tree for every object, with
  // a lot of redundancy and no sensible snipping. We can probably do a lot to optimize it.
  def descendants(root:OID, includeModels:Boolean, includeInstances:Boolean):Iterable[Thing] = {
    val candidates = 
      descendantsTyped(root, includeModels, includeInstances, types) ++
      descendantsTyped(root, includeModels, includeInstances, spaceProps) ++
      descendantsTyped(root, includeModels, includeInstances, things) ++
      descendantsTyped(root, includeModels, includeInstances, colls)
      
    val stripModels =
      if (includeModels)
        candidates
      else
        candidates.filterNot(_.isModel(this))
        
    val stripInstances =
      if (includeInstances)
        stripModels
      else
        stripModels.filter(_.isModel(this))
        
     stripInstances
  }
  
  /**
   * Given a Link Property, return all of the appropriate candidates for that property to point to.
   * 
   * The Property passed into here should usually be of LinkType -- while in theory that's not required,
   * it would be surprising for it to be used otherwise.
   */
  def linkCandidates(prop:Property[_,_]):Iterable[Thing] = {
    implicit val s = this
    
    val locals = linkCandidatesLocal(prop)
    if (app.isDefined && prop.hasProp(LinkAllowAppsProp) && prop.first(LinkAllowAppsProp))
      locals ++: app.get.linkCandidates(prop)
    else
      locals
  }

  /**
   * This enumerates all of the plausible candidates for the given property within this Space.
   */
  def linkCandidatesLocal(prop:Property[_,_]):Iterable[Thing] = {
    implicit val s = this
    
    // First, filter the candidates based on LinkKind:
    val allCandidates = if (prop.hasProp(LinkKindOID)) {
      val allowedKinds = prop.getPropVal(LinkKindProp).cv
      def fetchKind(wrappedVal:ElemValue):Iterable[Thing] = {
        val kind = LinkKindProp.pType.get(wrappedVal)
        kind match {
          case Kind.Thing => things.values
          case Kind.Property => spaceProps.values
          case Kind.Type => types.values
          case Kind.Collection => colls.values
          // TODO: distinguish Things and Attachments?
          case _ => Iterable.empty[Thing]
        }
      }
      (Iterable.empty[Thing] /: allowedKinds)((it, kind) => it ++: fetchKind(kind))
    } else {
      // No LinkKind specified, so figure that they only want Things:
      things.values
    }
    
    // Now, if they've specified a particular Model to be the limit of the candidate
    // tree -- essentially, they've specified what type you can link to -- filter for
    // that:
    val filteredByModel = if (prop.hasProp(LinkModelProp)) {
      val limit = prop.first(LinkModelProp)
      allCandidates filter (_.isAncestor(limit))
    } else {
      allCandidates
    }
    
    if (prop.ifSet(LinkToModelsOnlyProp)) {
      filteredByModel filter (_.isModel)
    } else {
      filteredByModel
    }
  }
  
  // TODO: this needs to become real. For now, everything is world-readable.
  def canRead(who:User):Boolean = {
    true
  }
  
  // TODO: this needs to become much more sophisticated. But for now, it's good enough
  // to say that only the owner can edit:
  def canCreateThings(who:User):Boolean = {
    who.id == owner
  }
  
  def canEdit(who:User, thingId:OID):Boolean = {
    querki.access.AccessControl.canEdit(this, who, thingId)
  }
  
  def renderUnknownName(implicit rc:controllers.RequestContext, name:String):Wikitext = {
    import ql._
    
    implicit val s = this
    val opt = getPropOpt(ShowUnknownProp)
    val nameVal = TypedValue(ExactlyOne(NameType(name)), NameType)
    val nameAsContext = QLContext(nameVal, rc)
    opt.map(pv => pv.render(nameAsContext)).getOrElse(Wikitext("Unknown Name: " + name))    
  }
}

