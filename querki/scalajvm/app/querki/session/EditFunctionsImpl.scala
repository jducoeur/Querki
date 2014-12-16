package querki.session

import scala.concurrent.{Future, Promise}

import akka.actor._

import models.{DisplayPropVal, DisplayText, FieldIds, FormFieldInfo, IndexedOID, Kind, PType, Thing, ThingId, Wikitext}
import models.Thing.{emptyProps, PropMap}

import querki.globals._

import querki.api.EditFunctions
import EditFunctions._
import querki.core.QLText
import querki.data._
import querki.session.messages.ChangeProps2
import querki.spaces.messages.{CreateThing, ThingFound, ThingError}
import querki.util.Requester
import querki.values.{QLRequestContext, RequestContext}

class EditFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends AutowireApiImpl(info, e) with EditFunctions {
  
  lazy val Basic = interface[querki.basic.Basic]
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Conventions = interface[querki.conventions.Conventions]
  lazy val Core = interface[querki.core.Core]
  lazy val DataModel = interface[querki.datamodel.DataModelAccess]
  lazy val Editor = interface[querki.editing.Editor]
  lazy val HtmlRenderer = interface[querki.html.HtmlRenderer]
  lazy val PropListManager = interface[querki.core.PropListManager]
  lazy val QL = interface[querki.ql.QL]
  lazy val Types = interface[querki.types.Types]
  
  lazy val doLogEdits = Config.getBoolean("querki.test.logEdits", false)
  
  def changeToProps(thing:Option[Thing], path:String, vs:Seq[String]):Option[PropMap] = {
    implicit val s = state
    DisplayPropVal.propPathFromName(path, thing).map { fieldIds =>
      // Compute the *actual* fields to change. Note that this isn't trivial, since the actual change might be in 
	  // a Bundle:
	  val context = QLRequestContext(rc)
	  val actualFormFieldInfo = HtmlRenderer.propValFromUser(fieldIds, vs.toList, context)
	  val result = fieldIds.container match {
	    // If this value is contained inside (potentially nested) Bundles, dive down into them
	    // and adjust the results:
        case Some(higherFieldIds) => {
          // TEMP: restructure rebuildBundle to take higherFieldIds directly:
          def toHigherIds(oneFieldId:FieldIds):List[IndexedOID] = {
            val thisId = IndexedOID(oneFieldId.p.id, oneFieldId.index)
            oneFieldId.container match {
              case Some(c) => toHigherIds(c) ::: List(thisId)
              case None => List(thisId)
            }
          }
          Types.rebuildBundle(thing, toHigherIds(higherFieldIds), actualFormFieldInfo).
            getOrElse(FormFieldInfo(fieldIds.p, None, true, false, None, Some(new PublicException("Didn't get bundle"))))         
        }
        case None => actualFormFieldInfo
	  }
	    
	  val FormFieldInfo(prop, value, _, _, _, _) = result
	  Core.toProps((prop, value.get))()
    }    
  }
  
  // TODO: this doesn't work with Lists that are nested in Models yet! Merge this with the above, but carefully. There
  // are common concepts of finding the Property, then creating the new value, then putting it into the right place.
  // But this may actually generalize to a broad concept of "change", as opposed to "replace".
  def alterListOrder(thing:Thing, path:String, from:Int, to:Int):Option[PropMap] = {
    implicit val s = state
    for {
      fieldIds <- DisplayPropVal.propPathFromName(path, Some(thing))
      prop = fieldIds.p
      pv <- thing.getPropOpt(prop)
      v = pv.v
      list = v.cv.toSeq
      if (list.isDefinedAt(from))
      elem = list(from)
      removed = list.patch(from, List(), 1)
      newList = removed.patch(to, List(elem), 0)
      newV = v.cType.makePropValue(newList, v.pType)
    }
      yield Core.toProps((prop, newV))()
  }
  
  def doChangeProps(thing:Thing, props:PropMap):Future[PropertyChangeResponse] = {
    val promise = Promise[PropertyChangeResponse]

    self.request(createSelfRequest(ChangeProps2(thing.toThingId, props))) {
      case ThingFound(_, _) => promise.success(PropertyChanged)
      
      // TODO: instead of PropertyChangeError, we really should have a generalized exception mechanism
      // at the autowire level, and do a promise.failure() here:
      case ThingError(ex, _) => promise.success(PropertyChangeError(ex.display(Some(rc))))
    }
    
    promise.future
  }
  
  def alterProperty(thingId:String, change:PropertyChange):Future[PropertyChangeResponse] = withThing(thingId) { thing =>
    if (doLogEdits) QLog.spew(s"Got alterProperty on $thingId: $change")
    
    val propsOpt:Option[PropMap] = change match {
      case ChangePropertyValue(path, vs) => changeToProps(Some(thing), path, vs)
      
      case MoveListItem(path, from, to) => alterListOrder(thing, path, from, to)
      
      case AddListItem(path) => {
        implicit val s = state
        for {
	      fieldIds <- DisplayPropVal.propPathFromName(path, Some(thing))
	      prop = fieldIds.p
	      if (prop.cType == Core.QList)
	      pt = prop.pType
	      pv <- thing.getPropOpt(prop)
	      v = pv.v
	      list = v.cv.toSeq
	      newI = list.size
          newElem = pt.default
          newList = list :+ newElem
          newV = v.cType.makePropValue(newList, pt)
        }
          yield Core.toProps((prop, newV))()        
      }
      
      case DeleteListItem(path, index) => {
        implicit val s = state
	    for {
	      fieldIds <- DisplayPropVal.propPathFromName(path, Some(thing))
	      prop = fieldIds.p
	      pv <- thing.getPropOpt(prop)
	      v = pv.v
	      list = v.cv.toSeq
	      if (list.isDefinedAt(index))
	      newList = list.patch(index, List(), 1)
	      newV = v.cType.makePropValue(newList, v.pType)
	    }
	      yield Core.toProps((prop, newV))()        
      }
      
      case DeleteProperty => None  // NYI
    }
    
    propsOpt match {
      case Some(props) => doChangeProps(thing, props)
      case None => Future.successful(PropertyChangeError("Unable to change property!"))
    }
  }
  
  def create(modelId:String, initialProps:Seq[PropertyChange]):Future[ThingInfo] = withThing(modelId) { model =>
    val promise = Promise[ThingInfo]
    
    val props = (emptyProps /: initialProps) { (map, change) =>
      change match {
        case ChangePropertyValue(path, vs) => changeToProps(None, path, vs) match {
          case Some(p) => map ++ p
          case None => throw new Exception(s"Invalid path $path")
        }
        case _ => throw new Exception(s"Invalid change for create $change")
      }
    }
    
    spaceRouter.request(CreateThing(user, state.owner, state.toThingId, model.kind, model.id, props)) {
      case ThingFound(thingId, newState) => {
        newState.anything(thingId) match {
          case Some(thing) => {
	        promise.success(ClientApi.thingInfo(thing, rc))
          }
          case None => promise.failure(new Exception("INTERNAL ERROR: Space claimed to create a new Thing, but seems to have failed?!?"))
        }
      }
      case ThingError(error, stateOpt) => promise.failure(error)
    }
    
    promise.future
  }
  
  private def getOnePropEditor(thing:Thing, prop:AnyProp, propVal:DisplayPropVal):PropEditInfo = {
    implicit val s = state
    val context = thing.thisAsContext(rc)
    val rendered = HtmlRenderer.renderPropertyInputStr(context, prop, propVal)
    PropEditInfo(
      ClientApi.propInfo(prop, rc),
      propVal.inputControlId,
      prop.getPropOpt(Editor.PromptProp).filter(!_.isEmpty).map(_.renderPlain),
      prop.getPropOpt(Conventions.PropSummary).map(_.render(prop.thisAsContext(rc))),
      propVal.inheritedFrom.map(_.displayName),
      rendered
      )
  }
  
  def getOnePropertyEditor(thingId:String, propId:String):PropEditInfo = withThing(thingId) { thing =>
    implicit val s = state
    val result = for {
      prop <- state.prop(ThingId(propId))
      pv = thing.getPropOpt(prop)
      v = pv.map(_.v)
      dpv = DisplayPropVal(Some(thing), prop, v)
    }
      yield getOnePropEditor(thing, prop, dpv)
    
    // TODO: produce a more useful Exception if this fails:
    result.get
  }
  
  def getPropertyEditors(thingId:String):FullEditInfo = withThing(thingId) { thing =>
    // Properties are very different from ordinary Things:
    thing match {
      case prop:AnyProp => getPropPropertyEditors(prop)
      case _ => getThingPropertyEditors(thing)
    }
  }
  
  private def getThingPropertyEditors(thing:Thing) = {
    implicit val s = state
    val model = thing.getModel
    val props = PropListManager.from(thing)
    val propList = PropListManager.prepPropList(props, Some(thing), model, state)
    
    val propInfos = propList.filter(_._1.id != querki.editing.MOIDs.InstanceEditPropsOID).map { entry =>
      val (prop, propVal) = entry
      getOnePropEditor(thing, prop, propVal)
    }
    
    val instanceProps = for {
      instancePropsPair <- propList.find(_._1.id == querki.editing.MOIDs.InstanceEditPropsOID)
      instancePropsQV <- instancePropsPair._2.v
      instanceProps = instancePropsQV.rawList(Core.LinkType)
    }
      yield instanceProps.map(_.toThingId.toString)
      
    val instancePropsPath = new FieldIds(Some(thing), Editor.InstanceProps).inputControlId
    
    FullEditInfo(instanceProps.getOrElse(Seq.empty), instancePropsPath, propInfos)
  }
  
  // TBD: this is pretty incestuous with the PropertyEditor in the client -- the list of available
  // props is defined here, in order. Not sure that's appropriate. Think about it...
  // TODO: someday, we should figure out what it means for a Property to have a Model. For now,
  // we're assuming that just doesn't happen.
  private def getPropPropertyEditors(prop:AnyProp):FullEditInfo = {
    implicit val s = state
    def onePropPair(propId:OID):(AnyProp, DisplayPropVal) = {
      val v = prop.props.get(propId)
      // TODO: we probably should have a better default here, to signal that this is an error:
      val subprop = state.prop(propId).getOrElse(Core.UrProp)
      (subprop, DisplayPropVal(Some(prop), subprop, v))
    }
    
    // These are meta-Properties whose AppliesToTypesProp points to this Type of Property.
    // Therefore, we want to show them in the Editor:
    val allMetapropsForTypeIds = 
      state.allProps.
      map(_._2).
      filter { tryProp =>
        val result = for {
          pv <- tryProp.getPropOpt(Types.AppliesToTypesProp)
          if (pv.contains(prop.pType))
        }
          yield true
        
        result.getOrElse(false)
      }.
      map(_.id).
      toSet
    
    // We *always* show Editors for Summary and Details -- they are recommended:
    val specialPropIds = Seq(Core.NameProp, Conventions.PropSummary, Conventions.PropDetails).map(_.id)
    val invariantPropIds = Seq(Core.TypeProp, Core.CollectionProp).map(_.id)
    val existingPropIds = ((prop.props.keys.toSet -- specialPropIds) -- invariantPropIds).toSeq
    val metaPropIds = (allMetapropsForTypeIds -- existingPropIds).toSeq
    
    // Specials are in their particular order; the rest get sorted by display:
    val specialProps = specialPropIds.map(onePropPair(_))
    val existingProps = existingPropIds.map(onePropPair(_))
    val metaProps = metaPropIds.map(onePropPair(_))
    val allProps = specialProps ++ (existingProps ++ metaProps).sortBy(_._2.prop.displayName)
    
    val allEditors = allProps.map(entry => getOnePropEditor(prop, entry._1, entry._2))
    // The Instance Property fields are meaningless for a Property:
    FullEditInfo(Seq.empty, "", allEditors)
  }
  
  def addPropertyAndGetEditor(thingId:String, propIdStr:String):Future[PropEditInfo] = withThing(thingId) { thing =>
    val promise = Promise[PropEditInfo]
    
    implicit val s = state
    val propId = ThingId(propIdStr)
    val propsOpt = for {
      prop <- state.prop(propId)
      newV = prop.default
    }
      yield Core.toProps((prop, newV))()  

    self.request(createSelfRequest(ChangeProps2(thing.toThingId, propsOpt.get))) {
      case ThingFound(id, newState) => {
        val result = for {
          newThing <- newState.anything(id)
          prop <- newState.prop(propId)
          pv <- newThing.getPropOpt(prop)
          propVal = DisplayPropVal(Some(newThing), prop, Some(pv.v))
        }
          yield getOnePropEditor(newThing, prop, propVal)
          
        promise.success(result.get)
      }
      
      case ThingError(ex, _) => promise.failure(ex)
    }
    
    promise.future    
  }
  
  def removeProperty(thingId:String, propIdStr:String):Future[PropertyChangeResponse] = withThing(thingId) { thing =>
    implicit val s = state
    val propId = ThingId(propIdStr)
    val propsOpt = for {
      prop <- state.prop(propId)
      newV = DataModel.DeletedValue
    }
      yield Core.toProps((prop, newV))()  

    propsOpt match {
      case Some(props) => doChangeProps(thing, props)
      case _ => Future.successful(PropertyChangeError(s"Couldn't find Property $propId!"))
    }
  }
  
  def getModelType(modelId:String):Future[TypeInfo] = withThing(modelId) { model =>
    implicit val s = state
    val typOpt = state.types.values.find { typ =>
      typ match {
        case mt:querki.types.ModelTypeBase => mt.basedOn == model.id
        case _ => false
      }
    }
    
    def toTypeInfo(t:PType[_]) = TypeInfo(t.id.toThingId, t.linkName, t.displayName)
    
    val promise = Promise[TypeInfo]
    typOpt match {
      case Some(typ) => promise.success(toTypeInfo(typ))
      case None => {
        val props = Map(
            Types.ModelForTypeProp(model),
            Basic.DisplayNameProp("__" + model.displayName + " Type"))
            
        // TODO: note that there is technically a slight race condition here -- between us fetching the SpaceState
        // in withThing(), and us sending this message, somebody else could create the new Type. That's not
        // devastating, but it *is* unintentional, and it's a good example of why we need to move towards a more
        // transactional view of things, where our CreateThing message includes the version stamp of the state it
        // is based on, and fails if the stamp is out of date.
        val spaceMsg = CreateThing(rc.requesterOrAnon, rc.ownerId, state.toThingId, Kind.Type, Core.UrType, props)
        
        spaceRouter.request(spaceMsg) {
          case ThingFound(typeId, newState) => {
            val typ = newState.typ(typeId)
            promise.success(toTypeInfo(typ))
          }
          case ThingError(error, stateOpt) => promise.failure(error)
        }
      }
    }
    promise.future
  }
}
