package querki.values

import language.existentials

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.math.Ordering

import models.{AnyProp, Collection, Property, PType, Thing, ThingState}
import models.{Kind}
import models.{AsName, AsOID, OID, ThingId}

import com.github.nscala_time.time.Imports._

import Thing.PropMap

import querki.core.NameUtils
import querki.ecology._
import querki.globals._
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
 */
case class SpaceState(
    s:OID, 
    m:OID,
    pf:PropMap,
    owner:OID,
    name:String,
    mt:DateTime,
    // Any Apps that this inherits from. These will be searched in-order, depth-first:
    apps:Seq[SpaceState],
    // The System State, which should be given unless this *is* the System State:
    system:Option[SpaceState],
    types:Map[OID, PType[_]],
    spaceProps:Map[OID, Property[_,_]],
    things:Map[OID, ThingState],
    colls:Map[OID, Collection],
    ownerIdentity:Option[querki.identity.Identity],
    cache:Map[StateCacheKey, Any] = Map.empty) 
  extends Thing(s, s, m, Kind.Space, pf, mt)
{
  override def toString = s"SpaceState '$toThingId' (${id.toThingId})"
  
  // *******************************************
  //
  // App-navigation functions
  //
  // These are general functions for "do something to this Space and all its Apps". They should, in
  // general, be used instead of working with apps from outside this SpaceState.
  //
  
  /**
   * The core function for looking something up in this Space. This tries
   * the given function in this Space, then recursively on all of the Apps.
   * (In-order, depth-first.) If none of that works, it tries the System Space.
   */
  def walkTree[R](f:(SpaceState) => Option[R]):Option[R] = {
    walkTreeRec(f).orElse(system.flatMap(f(_)))
  }
  
  /**
   * Look something up *only* in the Apps, not in this Space. This will include
   * System. You should usually use walkTree() instead.
   */
  def walkApps[R](f:(SpaceState) => Option[R]):Option[R] = {
    walkAppsRec(f).orElse(system.flatMap(f(_)))    
  }
  
  private def walkTreeRec[R](f: (SpaceState) => Option[R]):Option[R] = {
    f(this) match {
      case Some(result) => Some(result)
      case _ => walkAppsRec(f)
    }
  }
  
  private def walkAppsRec[R](f: (SpaceState) => Option[R]):Option[R] = {
    // Run through the apps, depth-first, looking for the result:
    (Option.empty[R] /: apps) { (cur, app) =>
      cur.orElse(app.walkTreeRec(f))
    }    
  }
  
  /**
   * Collect values from this Space and all of its Apps, including System.
   * 
   * This runs f on this State, then the Apps in-order, depth-first, and finally System.
   * 
   * NOTE: in a perfect world, we would simply be able to assume that accum is ++ and do
   * that. But I'm having trouble getting the types to line up right for that. I suspect
   * that the answer has something to do with Cats.Foldable.
   * 
   * IMPORTANT: this can result in duplication! It is essential that your implementation of
   * accum do any necessary de-duplication! It is often best to use this with Set or Map, which
   * do automatic de-duplication.
   * 
   * @tparam R An accumulation of values. Typically a collection, but doesn't have to be.
   * @param f A function that takes a SpaceState and produces an R result.
   * @param accum A fold function that takes two R's and flattens them into one.
   * 
   * @return The accumulated values from all of the SpaceStates.
   */
  def accumulateAll[R](f:SpaceState => R, accum:(R, R) => R):R = 
  {
    val fromTree = accumulateAllRec(f, accum)
    system match {
      case Some(s) => accum(fromTree, f(s))
      case _ => fromTree
    }
  }
  
  private def accumulateAllRec[R](f:SpaceState => R, accum:(R, R) => R):R = 
  {
    (f(this) /: apps) { (cur, app) =>
      val rest = app.accumulateAllRec(f, accum)
      accum(cur, rest)
    }
  }
  
  /**
   * A version of accumulateAll(), specialized for Maps.
   */
  def accumulateMaps[K,V](f:SpaceState => Map[K,V]):Map[K,V] = {
    // Note that, by the nature of Map, ++ automatically de-duplicates:
    accumulateAll(f, { (x:Map[K,V], y:Map[K,V]) => x ++ y })
  }
  
  // *******************************************
  
  // Walks up the App tree, looking for the specified Thing of the implied type:
  // IMPORTANT: note that the OID and ThingId versions of these methods are inconsistent in their
  // return signatures! That is a historical accident.
  // TODO: these OID methods really should return Option[T]. Throwing an exception this deep in the stack
  // is tending to produce pretty damned cryptic exceptions. Instead, just make the higher levels cope
  // with the Nones.
  def resolve[T <: Thing](tid:OID)(lookup: (SpaceState) => Map[OID, T]):T = {
    resolveOpt(tid)(lookup).getOrElse(throw new Exception("Couldn't find " + tid))
  }
  def resolveOpt[T <: Thing](tid:OID)(lookup: (SpaceState) => Map[OID, T]):Option[T] = {
    walkTree { state => lookup(state).get(tid) }
  }

  def typ(ptr:OID) = resolve(ptr) (_.types)
  def prop(ptr:OID) = resolveOpt(ptr) (_.spaceProps)
  def thing(ptr:OID) = resolve(ptr) (_.things)
  def coll(ptr:OID) = resolve(ptr) (_.colls)
  
  def resolve[T <: Thing](tid:ThingId)(lookup: (SpaceState) => Map[OID, T]):Option[T] = {
    walkTree { state =>
      val map = lookup(state)
      tid match {
        case AsOID(id) => map.get(id)
        case AsName(name) => thingWithName(name, map)
      }      
    }
  }
  def typ(ptr:ThingId) = resolve(ptr) (_.types)
  def prop(ptr:ThingId) = resolve(ptr) (_.spaceProps)
  def thing(ptr:ThingId) = resolve(ptr) (_.things)
  def coll(ptr:ThingId) = resolve(ptr) (_.colls)
  
  def anything(oid:OID):Option[Thing] = {
    walkTree { state =>
      state.things.get(oid).orElse(
        state.spaceProps.get(oid).orElse(
          state.types.get(oid).orElse(
            state.colls.get(oid).orElse(
              state.selfByOID(oid)))))
    }
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
    walkTree { state =>
      val name = rawName.toLowerCase()
      thingWithDisplayName(name, state.things).orElse(
        thingWithDisplayName(name, state.spaceProps).orElse(
          thingWithDisplayName(name, state.types).orElse(
            thingWithDisplayName(name, state.colls))))
    }
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
    walkTree { state =>
      val name = NameUtils.canonicalize(rawName)
      state.byCanonicalName.get(name)
    }.orElse(anythingByDisplayName(rawName))
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
   * Returns all of the conventional Things in this Space.
   */
  def localThings:Iterable[Thing] = things.values 
    
  /**
   * Returns all of the conventional Things in this Space *and* its Apps.
   */
  def allThings = accumulateAll[Set[Thing]]((_.things.values.toSet), (_ ++ _))
  
  def everythingLocal:Iterable[Thing] = things.values ++ spaceProps.values ++ types.values ++ colls.values
  def everything = accumulateAll[Set[Thing]]((_.everythingLocal.toSet), (_ ++ _))
  
  def propList:Iterable[Property[_,_]] = spaceProps.values
  def allProps:Map[OID, Property[_,_]] = accumulateMaps(_.spaceProps)   
  def allTypes:Map[OID, PType[_]] = accumulateMaps(_.types)
  
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
  
  def getApp(appId:OID):Option[SpaceState] = {
    walkTree { state =>
      if (appId == state.id)
        Some(state)
      else
        None
    }
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
  def fetchOrCreateCache[T](key:StateCacheKey, creator: => T):T = dynCache.getOrElseUpdate(key, creator).asInstanceOf[T]
  
  /* ////////////////////////////////////////////////////////////////////////
   * 
   * Inheritance Caching
   * 
   */
  
  /**
   * This is the pre-ordering that we do on Things at the cache level. This is *not* as good as
   * we do in _sort -- it doesn't take Computed Name into account -- but it is good enough for
   * most simple cases, and is about a zillion times faster.
   * 
   * It's a tad more complex than you'd expect at first glance, because multiple Things are allowed
   * to have the same Display Name, and must *not* be equal in compare(). (Because we are using
   * Set semantics -- equals means duplicate means dropped.) So we double-check the OIDs in that case.
   */
  private object SimpleThingOrdering extends Ordering[Thing] {
    def compare(x:Thing, y:Thing) = {
      val rawComp = x.unsafeDisplayName.toLowerCase.compare(y.unsafeDisplayName.toLowerCase)
      if (rawComp == 0)
        // Names match, so sort by OID. If the OIDs match, then they really are the same.
        x.id.raw.compare(y.id.raw)
      else
        rawComp
    }
  }
  
  /**
   * For each Thing that has children, this describes them.
   */
  case class ThingChildren(children:TreeSet[Thing])
  
  /**
   * The childrenMap is a lazily-computed map of inheritance within this Space -- for each Thing, what
   * Things inherit from it. We build and cache this for efficiency of looking up children, and for
   * pre-sorting them by name.
   */
  lazy val childrenMap:Map[OID, ThingChildren] = {
    (Map.empty[OID, ThingChildren] /: everythingLocal) { (curMap, t) =>
      val model = t.model
      val tc = curMap.get(model) match {
        case Some(thingChildren) => thingChildren.copy(children = thingChildren.children + t)
        case None => ThingChildren(TreeSet(t)(SimpleThingOrdering))
      }
      curMap + (model -> tc)
    }
  }
  
  /**
   * Returns all of the immediate children of this Thing.
   */
  def children(t:OID):SortedSet[Thing] = {
    childrenMap.get(t).map(_.children).getOrElse(SortedSet.empty(SimpleThingOrdering))
  }

  /**
   * Returns all of the immediate children of this Thing from *any* Space.
   */
  def allChildren(t:OID):SortedSet[Thing] = {
    accumulateAll(_.children(t), { (x:SortedSet[Thing], y:SortedSet[Thing]) => x ++ y })
  }

  /* ///////////////////////////////////////////////////////////////////// */
  
  def mapsize[T <: Thing](map:Map[OID, T]):Int = {
    map.values.map { v => 8 + v.memsize }.sum
  }
  /**
   * The total approximate size of this Space. See Thing.memsize for more info.
   */
  lazy val spaceSize:Int = {
    memsize + mapsize(things) + mapsize(spaceProps) + mapsize(types)
  }
  
  /**
   * This is a map from Things in Apps to their Shadows in this Space, so we can look that up
   * efficiently.
   */
  lazy val shadowMap:Map[OID,OID] = {
    // HACK: this is assuming that, if ShadowFlag is present, it is true
    val shadows = everythingLocal.filter { t => t.props.contains(querki.apps.MOIDs.ShadowFlagOID) }
    shadows.map { shadow =>
      (shadow.model -> shadow.id)
    }.toMap
  }
  
  def spaceStateOps(implicit e:Ecology) = new SpaceStateOps()(this, e)
}

class SpaceStateOps(implicit state:SpaceState, val ecology:Ecology) extends EcologyMember {
  def Core = interface[querki.core.Core]
  
  def things = state.things

  def models:Iterable[ThingState] = {
    implicit val s = this
    things.values.filter(_.first(Core.IsModelProp))    
  }
  def allModels:Iterable[ThingState] = {
    state.accumulateAll(_.models, { (x:Iterable[ThingState], y:Iterable[ThingState]) => x.toSet ++ y.toSet })
  }
  
  def descendantsRec(root:OID, includeApps:Boolean):SortedSet[Thing] = {
    val candidates = 
      if (includeApps)
        state.allChildren(root)
      else
        state.children(root)

    (candidates /: candidates) { (fullSet, child) =>
      fullSet ++ descendantsRec(child, includeApps)
    }
  }
  
  def descendants(root:OID, includeModels:Boolean, includeInstances:Boolean, includeApps:Boolean = false):SortedSet[Thing] = {
    val candidates = descendantsRec(root, includeApps)
      
    val stripModels =
      if (includeModels)
        candidates
      else
        candidates.filterNot(_.isModel)
        
    val stripInstances =
      if (includeInstances)
        stripModels
      else
        stripModels.filter(_.isModel)
        
     stripInstances
  }

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