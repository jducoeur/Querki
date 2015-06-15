package querki.values

import language.existentials

import models.{Collection, Property, PType, Thing, ThingState}
import models.{Kind}
import models.{AsName, AsOID, OID, ThingId}

import com.github.nscala_time.time.Imports._

import Thing.PropFetcher

import querki.core.NameUtils
import querki.ecology._

import querki.identity.User

import querki.util._
import querki.values._

/**
 * The keys for the SpaceState's cache. Each element is owned by a specific Ecot, which assigns
 * an arbitrary id.
 */
case class StateCacheKey(ecotId:Short, id:String)

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
    mt:DateTime,
    // TODO: in principle, this is a List[SpaceState] -- there can be multiple ancestors:
    app:Option[SpaceState],
    types:Map[OID, PType[_]],
    spaceProps:Map[OID, Property[_,_]],
    things:Map[OID, ThingState],
    colls:Map[OID, Collection],
    ownerIdentity:Option[querki.identity.Identity],
    e:Ecology,
    cache:Map[StateCacheKey, Any] = Map.empty) 
  extends Thing(s, s, m, Kind.Space, pf, mt)(e) with EcologyMemberBase[SpaceState, EcotImpl]
{
  lazy val Profiler = interface[querki.tools.Profiler]
  
  /**
   * Profiler Handle for use with the various Space Models. Other models are explicitly permitted to
   * use this Handle on an ad-hoc basis.
   */
  lazy val profilerHandle = Profiler.createHandle("SpaceState")
  
  override def toString = s"SpaceState '$toThingId' (${id.toThingId})"
  
  // *******************************************
  
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
  // TODO: this is what we should be using instead of the old resolve()..
  def resolveOpt[T <: Thing](tid:OID)(lookup: (SpaceState) => Map[OID, T]):Option[T] = {
    lookup(this).get(tid).orElse(
          app.flatMap(_.resolveOpt(tid)(lookup)))
  }
  def typ(ptr:OID) = resolve(ptr) (_.types)
  def prop(ptr:OID) = resolveOpt(ptr) (_.spaceProps)
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
    things.get(oid).orElse(
      spaceProps.get(oid).orElse(
        types.get(oid).orElse(
          colls.get(oid).orElse(
            selfByOID(oid).orElse(
              app.flatMap(_.anything(oid)))))))
  }
  
  private def thingWithName[T <: Thing](name:String, things:Map[OID, T]):Option[T] = {
    things.values.find { thing =>
      val thingNameOpt = thing.canonicalName
      thingNameOpt.isDefined && NameUtils.equalNames(thingNameOpt.get, name)
    }
  }
  
  private def thingWithDisplayName[T <: Thing](name:String, things:Map[OID, T]):Option[T] = {
    things.values.find { thing =>
      thing.unsafeDisplayName.toLowerCase() == name
    }
  }
  
  def selfByOID(oid:OID):Option[Thing] = {
    if (oid == id) Some(this) else None
  }
  def spaceByName(tryName:String):Option[Thing] = {
    if (tryName == NameUtils.toInternal(name)) Some(this) else None
  }
  
  def anythingByDisplayName(rawName:String):Option[Thing] = {
    val name = rawName.toLowerCase()
    thingWithDisplayName(name, things).orElse(
      thingWithDisplayName(name, spaceProps).orElse(
        thingWithDisplayName(name, types).orElse(
          thingWithDisplayName(name, colls).orElse(
            app.flatMap(_.anythingByDisplayName(rawName))))))
  }
  
  /**
   * We maintain an internal lazy map of all the Thing in this Space, by their Canonical name.
   * This is specifically to make name lookups decently efficient, since we do that a *lot* when
   * processing QL.
   * 
   * Note that, since this is lazy, it will get regenerated every time we version the Space. A
   * tad expensive, but a hell of a lot cheaper than doing a longhand search by name every time.
   */ 
  private lazy val byCanonicalName:Map[String,Thing] = {
    def addTable[T <: Thing](things:Map[OID, T]):Map[String,T] = {
      val pairs = things.values.map { thing =>
        thing.canonicalName.map((NameUtils.canonicalize(_) -> thing))
      }.flatten.toSeq
      Map(pairs:_*)
    }
    
    addTable(things) ++
    addTable(spaceProps) ++
    addTable(types) ++
    addTable(colls) +
    (NameUtils.canonicalize(canonicalName.get) -> this)
  }
  
  def anythingByName(rawName:String):Option[Thing] = {
    val name = NameUtils.canonicalize(rawName)
    byCanonicalName.get(name).orElse(app.flatMap(_.anythingByName(rawName))).orElse(anythingByDisplayName(rawName))
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
  
  def propList:Iterable[Property[_,_]] = spaceProps.values
  def allProps:Map[OID, Property[_,_]] = if (app.isEmpty) spaceProps else spaceProps ++ app.get.allProps
  
  def allTypes:Map[OID, PType[_]] = if (app.isEmpty) types else types ++ app.get.types

  def models:Iterable[ThingState] = {
    implicit val s = this
    things.values.filter(_.first(Core.IsModelProp))    
  }
  def allModels:Iterable[ThingState] = {
    val myModels = models
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
  
  def thingsWithProp(prop:Property[_,_]):Iterable[Thing] = {
    everythingLocal.filter(_.hasProp(prop.id)(this))
  }
  
  def propsOfType[VT](pt:PType[VT]):Iterable[Property[VT,_]] = {
    spaceProps.values.filter(_.pType == pt).map(_.confirmType(pt).get)
  }
  
  /**
   * Returns all of the immediate children of this Thing.
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
  
  def getApp(appId:OID):Option[SpaceState] = {
    if (appId == id)
      Some(this)
    else
      app.flatMap(_.getApp(appId))
  }
  
  /***************************************
   * The Dynamic Cache
   * 
   * This is a secondary cache, constructed very differently from the one in the main case class
   * values. Whereas the one at the top is built during Space modification, by the Space itself,
   * this one permits the more dangerous but easier to use direct modification by subsystems when
   * needed.
   * 
   * The idea here is that we have a lot of things we'd like to cache lazily on-demand, rather than
   * every time the SpaceState is modified. So we allow that here, hedged with some big warnings.
   * 
   * All values here *MUST* be managed in a threadsafe way. By and large, we expect this to be
   * populated by subsidiary concurrent.Maps. Keep in mind that any value in here might be modified
   * by multiple UserSpaceSessions simultaneously.
   * 
   * Since this is a lazy val, it will not be preserved when the SpaceState is copied. Values in here
   * should be caches of expensive computations on the current SpaceState.
   * 
   * DO NOT USE THIS CASUALLY! This is trading memory for speed. Sometimes it is very valuable, but
   * only use it when there is a high likelihood of a common speedup. 
   */
  private lazy val dynCache = scala.collection.concurrent.TrieMap.empty[StateCacheKey, Any]
  def fetchOrCreateCache(key:StateCacheKey, creator: => Any):Any = dynCache.getOrElseUpdate(key, creator)
}

object SpaceState {
  
  /**
   * Extra functionality that is sometimes useful to consider as part of the state, but isn't
   * really part of the core concept. Factored out to keep the main SpaceState interface and dependencies decently clean.
   * 
   * By and large, if you find yourself tempted to add new dependencies to SpaceState, consider putting them
   * here instead.
   */
  implicit class SpaceStateExtras(state:SpaceState) {
    def ownerName:String = state.ownerIdentity.map(_.name).getOrElse(state.owner.toThingId)
    def ownerHandle:String = state.ownerIdentity.map(_.handle).getOrElse(state.owner.toThingId)
  }
}