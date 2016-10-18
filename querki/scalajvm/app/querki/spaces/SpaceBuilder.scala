package querki.spaces

import scala.annotation.tailrec

import akka.actor._

import org.querki.requester._

import models._

import querki.globals._
import querki.identity.User
import querki.spaces.messages._
import querki.types._
import querki.values.{ElemValue, RequestContext, QValue}

/**
 * Mix-in trait for building a new Space from a SpaceState that describes the desired outcome.
 * 
 * TODO: this should be rewritten to be smarter for the new Akka Persistence world. We can make
 * this *vastly* more efficient by:
 * 
 * a) Adding the ability to request an entire block of OIDs at a shot.
 * b) In one operation, requesting a block of OIDs equal to the number of Things in this Space.
 * c) Do the entire ID mapping and rebuilding operation at one shot.
 * d) Assigning the entire new Space as a single BootSpace operation.
 * 
 * That would take just a couple of roundtrips, instead of hundreds-to-thousands.
 * 
 * @author jducoeur
 */
trait SpaceBuilder { anActor:Actor with Requester with EcologyMember =>
  
  import SpaceBuilder._
  
  // The concrete implementation must implement these:
  def setMsg(msg:String):Unit
  def setTotalThingOps(n:Int)
  def incThingOps():Unit
  def createMsg:String
  def buildingMsg:String
  def thingsMsg:String
  def typesMsg:String
  def propsMsg:String
  def valuesMsg:String
  
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Core = interface[querki.core.Core]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  lazy val SpacePersistenceFactory = interface[querki.spaces.SpacePersistenceFactory]
  lazy val Types = interface[querki.types.Types]
  
  lazy val LinkType = Core.LinkType
  
  lazy val useNewPersist = Config.getBoolean("querki.space.newPersist", false)
  
  def startSpaceActor(spaceId:OID):ActorRef = {
//    if (useNewPersist)
      context.actorOf(PersistentSpaceActor.actorProps(ecology, SpacePersistenceFactory, self, spaceId), "Space")
//    else
//      context.actorOf(Space.actorProps(ecology, SpacePersistenceFactory, self, spaceId), "Space")
  }
  
  /**
   * The importers start by building a faux SpaceState, representing the data to import. Now, we
   * actually build up a real Space from that. The returned Future will complete once the new
   * Space is fully constructed and persisted.
   */
  def buildSpace(user:User, name:String)(implicit imp:SpaceState):Future[NewSpaceInfo] = {
    // Note that this whole process is a really huge RequestM composition, and it isn't as pure
    // functional as it might look -- there are a bunch of side-effecting data structures involved,
    // for simplicity.
    setMsg(createMsg)
    for {
      // First, we create the new Space:
      info @ SpaceInfo(spaceId, canon, display, ownerHandle) <- SpaceOps.spaceManager.request(CreateSpace(user, name, StatusInitializing))
      // IMPORTANT: we are creating a *local* version of the Space Actor, under the aegis of this
      // request, for purposes of this setup process. Afterwards, we'll shut this Actor down and hand
      // off to more normal machinery.
      // TODO: how can we intercept all failures, and make sure that this Actor gets shut down if
      // something goes wrong? Keep in mind that the process is wildly async, so we can't just do
      // try/catch.
      dummyMsg = setMsg(buildingMsg)
      spaceActor = startSpaceActor(spaceId)
      // Create all of the Models and Instances, so we have all their OIDs in the map.
      dummyMsg2 = {
        setMsg(thingsMsg)
        setTotalThingOps(
          (imp.things.size * 2) +
          imp.types.size +
          imp.spaceProps.size
        )
      }
      initfp = FoldParams(user, spaceActor, spaceId, Map.empty, None) + (imp.id -> spaceId)
      pWithThings <- createThings(initfp, sortThings(imp))
      dummyMsg3 = setMsg(typesMsg)
      pWithTypes <- createModelTypes(pWithThings, imp.types.values.toSeq)
      dummyMsg4 = setMsg(propsMsg)
      pWithProps <- createProperties(pWithTypes, imp.spaceProps.values.toSeq)
      dummyMsg5 = setMsg(valuesMsg)
      pWithValues <- setPropValues(pWithProps, imp.things.values.toSeq)
      dummy2 <- setSpaceProps(pWithValues)
      // Finally, once it's all built, shut down this temp Actor:
      dummy3 = context.stop(spaceActor)
      // And tell the SpaceManager that this Space is now ready to be treated normally:
      _ <- SpaceOps.spaceManager.requestFor[StatusChanged.type](ChangeSpaceStatus(spaceId, StatusNormal))
    }
      yield NewSpaceInfo(info, pWithValues.idMap)
  }
  
  /*****************************
   * Topological Sort of the Model Hierarchy
   */
  
  /**
   * Generic topological sort algorithm.
   */
  def tsort[A](edges: Traversable[(A, A)]): Iterable[A] = {
    @tailrec
    def tsort(toPreds: Map[A, Set[A]], done: Iterable[A]): Iterable[A] = {
        val (noPreds, hasPreds) = toPreds.partition { _._2.isEmpty }
        if (noPreds.isEmpty) {
            if (hasPreds.isEmpty) done else sys.error(hasPreds.toString)
        } else {
            val found = noPreds.map { _._1 }
            tsort(hasPreds.mapValues { _ -- found }, done ++ found)    
        }
    }

    val toPred = edges.foldLeft(Map[A, Set[A]]()) { (acc, e) =>
        acc + (e._1 -> acc.getOrElse(e._1, Set())) + (e._2 -> (acc.getOrElse(e._2, Set()) + e._1))
    }
    tsort(toPred, Seq())
  }
  
  def sortThings(space:SpaceState):Seq[Thing] = {
    val allThings = space.things.values
    val edges = allThings.map { thing => (thing.model, thing.id) }
    val sortedOIDs = tsort(edges)
    // Need to allow for the fact that the sortedOIDs contains models that weren't in the
    // original list, because they are from the Apps:
    sortedOIDs.map(space.things.get(_)).flatten.toSeq
  }
  
  /***************************/
  
  def idMapOr(id:OID, idMap:IDMap)(implicit imp:SpaceState):OID = {
    idMap.get(id) match {
      case Some(m) => m
      case None => {
        // The value wasn't found in the mapped IDs. Does it exist in the Apps? (Which are real Spaces with real IDs.)
        val mOpt = imp.walkApps { app => 
          app.anything(id)
        }
         
        mOpt.map(_.id).getOrElse(UnknownOID)
      }
    }
  }
  def model(t:Thing, idMap:IDMap)(implicit imp:SpaceState):OID = {
    try {
      idMapOr(t.model, idMap)
    } catch {
      case ex:Exception => {
        QLog.spewThing(t)
        throw ex
      } 
    }
  }
  
  case class FoldParams(user:User, actor:ActorRef, spaceId:OID, idMap:IDMap, realSpaceOpt:Option[SpaceState]) {
    def updateAnd(f: => FoldParams):FoldParams = {
      incThingOps()
      f
    }
    
    def +(mapping:(OID, OID)) = copy(idMap = idMap + mapping)
    def +(space:SpaceState) = copy(realSpaceOpt = Some(space))
    def +(found:ThingFound):FoldParams = {
      val ThingFound(intId, state) = found
      updateAnd(this + state)
    }
    def +(extId:ExtId, found:ThingFound):FoldParams = {
      val ThingFound(intId, state) = found
      updateAnd(this + state + (extId -> intId))
    }
    def space = realSpaceOpt.get
  }

  /**
   * Step 1: create all of the Models and Instances, empty, so that we have their real OIDs.
   */
  private def createThings(p:FoldParams, things:Seq[Thing])(implicit imp:SpaceState):RequestM[FoldParams] = {
    things.headOption match {
      case Some(thing) => {
        p.actor.request(CreateThing(p.user, p.spaceId, Kind.Thing, model(thing, p.idMap), Thing.emptyProps)) flatMap {
          case found @ ThingFound(intId, state) => {
            // QLog.spew(s"Mapped Thing ${thing.id} -> $intId")
            createThings(p + (thing.id, found), things.tail)
          }
          case ThingError(ex, _) => {
            RequestM.failed(ex)
          }
        }
      }
      case None => { QLog.spew(s"Done creating the Things"); RequestM.successful(p) }
    }    
  }
  
  /**
   * Step 2: create all of the Model Types.
   */
  private def createModelTypes(p:FoldParams, modelTypes:Seq[PType[_]])(implicit imp:SpaceState):RequestM[FoldParams] = {
    modelTypes.headOption match {
      case Some(pt) => {
        pt match {
          case mt:ModelTypeBase => {
            // Translate the link to the underlying Model:
            val props = mt.props + (Types.ModelForTypeProp(p.idMap(mt.basedOn)))
            p.actor.request(CreateThing(p.user, p.spaceId, Kind.Type, Core.UrType, props)) flatMap {
              case found @ ThingFound(intId, state) => {
                // QLog.spew(s"Mapped Model Type ${pt.id} -> $intId")
                createModelTypes(p + (mt.id, found), modelTypes.tail)
              }
              case ThingError(ex, _) => {
                RequestM.failed(ex)
              }
            }
          }
          case _ => {
            QLog.error(s"Somehow trying to import a non-Model Type: $pt")
            createThings(p, modelTypes.tail)
          }
        }
      }
      case None => RequestM.successful(p)
    }
  }
  
  // These are the Properties whose values can not be set yet, indexed by the ID of their Model Type:
  // TODO: is this sufficient? What if this Property is based on a Model Type that is itself based on
  // three more Model Types?
  private var deferredPropertiesByModel = Map.empty[ExtId, Seq[ExtId]]
  
  // These are the Properties whose values can not be set yet, by PropertyId:
  private var deferredProperties = Set.empty[ExtId]
  
  // These are the values waiting on deferred Properties:
  case class DeferredPropertyValue(propId:ExtId, thingId:ExtId, v:QValue) {
    
  }
  private var deferredPropertyValues = Map.empty[ExtId, Seq[DeferredPropertyValue]]
  
  /**
   * Step 3: create all of the Properties.
   */
  private def createProperties(p:FoldParams, props:Seq[AnyProp])(implicit imp:SpaceState):RequestM[FoldParams] = {
    props.headOption match {
      case Some(prop) => {
        //QLog.spew(s"Creating Property ${prop.displayName}")
        //QLog.spewThing(prop)
        // Take note of any Properties that can't be resolved yet:
        prop.pType match {
          case mt:ModelTypeBase if (p.idMap.contains(mt.basedOn)) => {
            // QLog.spew(s"  Deferring property ${prop.displayName} (${prop.id}), which is based on ${mt.basedOn}")
            val propsForThisModel = deferredPropertiesByModel.get(mt.basedOn) match {
              case Some(otherProps) => otherProps :+ prop.id
              case None => Seq(prop.id)
            }
            deferredPropertiesByModel += (mt.basedOn -> propsForThisModel)
            deferredProperties += prop.id
          }
          case _ => 
        }
        
        // Note that we are currently assuming you aren't defining meta-Properties that you are putting on
        // your Properties; if a Property depends on another local Property, we may have order problems.
        // TODO: cope with this edge case. In that case, we will need to add those in another pass, or something.
        // Regardless of this, we still need to call translateProps, to deal with Link meta-Properties.
        p.actor.request(CreateThing(p.user, p.spaceId, Kind.Property, querki.core.MOIDs.UrPropOID, translateProps(prop, p))) flatMap {
          case found @ ThingFound(intId, state) => {
            // QLog.spew(s"Mapped Property ${prop.id} -> $intId")
            createProperties(p + (prop.id, found), props.tail)
          }
          case ThingError(ex, _) => {
            RequestM.failed(ex)
          }
        }         
      }
      case None => RequestM.successful(p)
    }
  }
  
  private def translateProp(pair:(OID, QValue), p:FoldParams)(implicit imp:SpaceState):(OID, QValue) = {
    val (propId, qv) = pair
    val realQv = imp.prop(propId) match {
      case Some(prop) => {
        prop.pType match {
          case pt:querki.core.IsLinkType => {
            // This Property is made of Links, so we need to map all of them to the new values:
            val raw = qv.rawList(Core.LinkType)
            val translatedLinks = raw.map { extId => ElemValue(idMapOr(extId, p.idMap), Core.LinkType) }
            //QLog.spew(s"Translated $raw to $translatedLinks")
            qv.cType.makePropValue(translatedLinks, Core.LinkType)
          }
          case mt:ModelTypeBase => {
            if (deferredProperties.contains(propId)) {
              qv
            } else {
              // Okay -- this Property is based on a Model Type value that has been resolved. 
              // We need to change the Type of each Element to the correct Type.
              // TODO: this can still fail on nested Model Types, because we're not necessarily
              // waiting long enough to resolve the inner Types!
              val realType = p.realSpaceOpt.get.typ(idMapOr(mt.id, p.idMap)).asInstanceOf[ModelTypeBase]
              val translatedModels = for {
                elem <- qv.cv
                bundle = mt.get(elem)
                transProps = bundle.props.map(translateProp(_, p))
                simpleBundle = SimplePropertyBundle(transProps)
                modelValue = realType(simpleBundle)
              }
                yield modelValue
              val result = qv.cType.makePropValue(translatedModels, realType)
              result
            }
          }
          case _ => qv
        }              
      }
      case None => qv  // Property not found! TODO: This is weird and buggy! What should we do with it?
    }
      
    // Try translating, but it is very normal for the propId to be from System:
    (idMapOr(propId, p.idMap), realQv)
  }
  
  /**
   * This looks at the properties of t. It adds a deferral for any Properties that we
   * can't yet deal with, and returns the rest. It also translates the Property IDs.
   * 
   * TODO: this is not yet sufficient for dealing with Model Types. If it's a Model Type
   * value, we need to dive into it and translate all of the nested values in there.
   */
  private def translateProps(pb:PropertyBundle, p:FoldParams)(implicit imp:SpaceState):Thing.PropMap = {
    val translated = pb.props filter { pair =>
      val (propId, qv) = pair
      if (deferredProperties.contains(propId)) {
        pb match {
          case t:Thing => {
            val deferredVals = deferredPropertyValues.get(propId) match {
              case Some(vals) => vals :+ DeferredPropertyValue(propId, t.id, qv)
              case None => Seq(DeferredPropertyValue(propId, t.id, qv))
            }
            deferredPropertyValues += (propId -> deferredVals)
//            QLog.spew(s"Deferring property $propId with value $qv")
          }
          case _ => {
            // TODO: we need to handle this situation. We need enough information to be able to
            // defer this value and fix it up later:
            QLog.warn(s"Found deferred property $propId in non-Thing $pb; dropping it on the floor!")
          }
        }
        false
      } else
        true
    } map { pair =>
      translateProp(pair, p)
    }
    
    translated
  }
  
  /**
   * Step 4a: when we resolve a Model that is the basis of a Property, we divert to here to
   * resolve any deferred Property Values that might have been waiting on it.
   */
  private def fixDeferrals(p:FoldParams, deferrals:Seq[DeferredPropertyValue])(implicit imp:SpaceState):RequestM[FoldParams] = {
    deferrals.headOption match {
      case Some(deferral) => {
//        QLog.spew(s"Fixing deferral $deferral")

        // Okay, we've got a deferral of a Model Value. We need to dig into it, rewriting the ID
        // of the Property, the ID of the Type, and all of the Property IDs *inside* the value.
        // TODO: this needs to become more sophisticated -- we need to be able to defer full paths
        // into nested structures:
        val DeferredPropertyValue(propId:ExtId, thingId:ExtId, v:QValue) = deferral
        val intPID = p.idMap(propId)
        // The original Property:
        val extProp = imp.prop(propId).get
        // The original Model Type, that we found earlier for this Property:
        val extType = extProp.pType.asInstanceOf[ModelTypeBase]
        // The original Model Values:
        val extVals = v.rawList(extType)
        // The new Model Type:
        // This is rather suspicious, although it probably works:
        val intType = p.space.typ(p.idMap(extType.id)).asInstanceOf[ModelTypeBase]
        // The new Model Values:
        val intVals = extVals map { extVal =>
          val intVal = translateProps(extVal, p)
          intType(vals2Bundle(intVal.toSeq:_*))
        }
        // The composed new QValue with the new Model Values:
        val intV = v.cType.makePropValue(intVals, intType)
        
        p.actor.request(ChangeProps(p.user, p.spaceId, p.idMap(thingId), Map((intPID -> intV)), true)) flatMap {
          case found @ ThingFound(intId, state) => {
            fixDeferrals(p + found, deferrals.tail)
          }
          case ThingError(ex, _) => RequestM.failed(ex)
        }
      }
      
      case None => RequestM.successful(p)
    }
  }
  
  /**
   * Step 4: Okay, everything is created; now fill in the values.
   */
  private def setPropValues(p:FoldParams, things:Seq[Thing])(implicit imp:SpaceState):RequestM[FoldParams] = {
    things.headOption match {
      case Some(thing) => {
//        QLog.spew(s"Setting Property values on")
//        QLog.spewThing(thing)(imp)
        p.actor.request(ChangeProps(p.user, p.spaceId, p.idMap(thing.id), translateProps(thing, p), true)) flatMap {
          case found @ ThingFound(intId, state) => {
            if (deferredPropertiesByModel.contains(thing.id)) {
//              QLog.spew(s"${thing.displayName} is a Model with deferrals")
              // This is apparently a Model for a Model Type. So now it's time to go deal with any
              // Property Values that have been waiting on it, before we move on:
              val props = deferredPropertiesByModel(thing.id)
              val propVals = for {
                propId <- props
                propVals = deferredPropertyValues.get(propId).getOrElse(Seq.empty)
                propVal <- propVals
              }
                yield propVal
                
              deferredPropertiesByModel -= thing.id
              props.foreach { propId => 
                deferredProperties -= propId
                deferredPropertyValues -= propId
              }
              fixDeferrals(p, propVals) flatMap { dummy => 
                setPropValues(p + found, things.tail)
              }
            } else {
              // Ordinary Thing, so continue on...
              setPropValues(p + found, things.tail)
            }
          }
          case ThingError(ex, _) => {
            RequestM.failed(ex)
          }
        }
      }
      case None => RequestM.successful(p)
    }
  }
  
  /**
   * We specifically do *not* want to import the space's old Name or Display Name.
   */
  def filterSpaceProps(props:Thing.PropMap):Thing.PropMap = {
    props filter { pair =>
      val (propId, qv) = pair
      propId != Core.NameProp.id && propId != Basic.DisplayNameProp.id
    }
  }
  
  /**
   * Step 5: set the properties on the Space itself.
   */
  private def setSpaceProps(p:FoldParams)(implicit imp:SpaceState):RequestM[Any] = {
    p.actor.request(ChangeProps(p.user, p.spaceId, p.spaceId, filterSpaceProps(translateProps(imp, p)), true))
  }
  
}

object SpaceBuilder {
  // Types used by SpaceBuilder and NewSpaceInfo. These represent the "external" (original) and
  // "internal" (newly constructed) OIDs in the new Space:
  type ExtId = OID
  type IntId = OID
  type IDMap = Map[ExtId, IntId]
  
  /**
   * The results of constructing a new Space using SpaceBuilder.
   */
  case class NewSpaceInfo(info:SpaceInfo, mapping:IDMap)
}