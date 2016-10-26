package querki.editing

import akka.actor._

import models.{DisplayPropVal, FieldIds, FormFieldInfo, IndexedOID, Kind, PType, Thing}
import models.Thing.{emptyProps, PropMap}

import querki.api.{SpaceApiImpl, AutowireParams, OperationHandle, ProgressActor}
import querki.globals._
import querki.data._
import EditFunctions._
import querki.session.messages.ChangeProps2
import querki.spaces.messages.{CreateThing, ModifyThing, ThingFound, ThingError}
import querki.util.{PublicException}
import querki.values.QLRequestContext
import models.OID.thing2OID
import models.Thing.thing2Ops
import models.ThingId.thingId2Str

class EditFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends SpaceApiImpl(info, e) with EditFunctions {
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Conventions = interface[querki.conventions.Conventions]
  lazy val Core = interface[querki.core.Core]
  lazy val DataModel = interface[querki.datamodel.DataModelAccess]
  lazy val DeriveName = interface[querki.types.DeriveName]
  lazy val Editor = interface[querki.editing.Editor]
  lazy val HtmlRenderer = interface[querki.html.HtmlRenderer]
  lazy val PropListManager = interface[querki.core.PropListManager]
  lazy val QL = interface[querki.ql.QL]
  lazy val Tags = interface[querki.tags.Tags]
  lazy val Types = interface[querki.types.Types]
  
  lazy val doLogEdits = Config.getBoolean("querki.test.logEdits", false)
  
  def doRoute(req:Request):Future[String] = route[EditFunctions](this)(req)
  
  // TODO: this should really return a Try, or something like that, returning a more explicit Exception.
  // Getting None here means that the given value failed validation.
  // TODO: do a much finer-grained validation here, and return more-precise errors.
  def changeToProps(thing:Option[Thing], path:String, vs:Seq[String]):Option[PropMap] = {
    implicit val s = state
    DisplayPropVal.propPathFromName(path, thing).flatMap { fieldIds =>
      // Compute the *actual* fields to change. Note that this isn't trivial, since the actual change might be in 
  	  // a Bundle:
  	  val context = QLRequestContext(rc)
  	  val actualFormFieldInfo = HtmlRenderer.propValFromUser(fieldIds, vs.toList, context)
  	  if (!actualFormFieldInfo.isValid){
  	    val msg = actualFormFieldInfo.error.map(_.display(Some(rc))).getOrElse("Validation Error")
  	    throw new querki.api.ValidationException(msg)
  	  }
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
  	  // Note that value can be empty if it fails validation!!!
  	  value.map(v => Core.toProps((prop, v)))
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
      yield Core.toProps((prop, newV))
  }
  
  def doChangeProps(thing:Thing, props:PropMap):Future[PropertyChangeResponse] = {
    self.request(createSelfRequest(ChangeProps2(thing.toThingId, props))) map {
      case ThingFound(_, _) => PropertyChanged
      case ThingError(ex, _) => throw new querki.api.GeneralChangeFailure("Error during save")
    }
  }
  
  def alterProperty(thingId:TID, change:PropertyChange):Future[PropertyChangeResponse] = withThing(thingId) { thing =>
    if (doLogEdits) QLog.spew(s"Got alterProperty on $thingId: $change")
    implicit val s = state
    
    val propsOpt:Option[PropMap] = change match {
      case ChangePropertyValue(path, vs) => changeToProps(Some(thing), path, vs)
      
      case MoveListItem(path, from, to) => alterListOrder(thing, path, from, to)
      
      // TODO: figure out the right refactoring for the clauses below. They're very similar, but vary
      // in multiple dimensions:
      case AddListItem(path) => {
        for {
	      fieldIds <- DisplayPropVal.propPathFromName(path, Some(thing))
	      prop = fieldIds.p
	      if ((prop.cType == Core.QList) || (prop.cType == Core.QSet))
	      pt = prop.pType
	      pv <- thing.getPropOpt(prop)
	      v = pv.v
	      list = v.cv.toSeq
	      newI = list.size
          newElem = pt.default
          newList = list :+ newElem
          newV = v.cType.makePropValue(newList, pt)
        }
          yield Core.toProps((prop, newV))      
      }
      
      case DeleteListItem(path, index) => {
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
	      yield Core.toProps((prop, newV))
      }
      
      case AddToSet(path, value) => {
        for {
          fieldIds <- DisplayPropVal.propPathFromName(path, Some(thing))
          prop = fieldIds.p
          pt = prop.pType
          pv <- thing.getPropOpt(prop)
          v = pv.v
	      list = v.cv.toSeq
          newElem = pt.fromUser(value)
          newList = list :+ newElem
          newV = v.cType.makePropValue(newList, pt)
        }
          yield Core.toProps((prop, newV))
      }
      
      case RemoveFromSet(path, value) => {
        for {
          fieldIds <- DisplayPropVal.propPathFromName(path, Some(thing))
          prop = fieldIds.p
          pt = prop.pType
          pv <- thing.getPropOpt(prop)
          v = pv.v
	      list = v.cv.toSeq
          deadElem = pt.fromUser(value)
          newList = list.filterNot(pt.matches(_, deadElem))
          newV = v.cType.makePropValue(newList, pt)
        }
          yield Core.toProps((prop, newV))
      }
    }
    
    propsOpt match {
      case Some(props) => doChangeProps(thing, props)
      case None => Future.failed(new querki.api.GeneralChangeFailure("Error during save"))
    }
  }
  
  def create(modelId:TID, initialProps:Seq[PropertyChange]):Future[ThingInfo] = withThing(modelId) { model =>
    implicit val s = state
    val propsFromClient = (emptyProps /: initialProps) { (map, change) =>
      change match {
        case ChangePropertyValue(path, vs) => changeToProps(None, path, vs) match {
          case Some(p) => map ++ p
          case None => throw new Exception(s"Invalid path $path")
        }
        case _ => throw new Exception(s"Invalid change for create $change")
      }
    }
    
    val creatingModel = propsFromClient.get(querki.core.MOIDs.IsModelOID) match {
      case Some(qv) => qv.firstAs(Core.YesNoType).getOrElse(false)
      case _ => false
    }
    
    val props = 
      if (creatingModel) {
        // When creating a sub-Model, copy its parent's Instance Properties:
        model.getPropOpt(Editor.InstanceProps) match {
          case Some(instanceProps) => propsFromClient + (Editor.InstanceProps.id -> instanceProps.v)
          // The parent didn't have any Instance Properties, so create the minimal list:
          case None => propsFromClient + (Editor.InstanceProps(Basic.DisplayNameProp.id))
        }
      } else
        // Creating an Instance, so don't do anything special:
        propsFromClient
    
    spaceRouter.request(CreateThing(user, state.id, model.kind, model.id, props)) flatMap {
      case ThingFound(thingId, newState) => {
        newState.anything(thingId) match {
          case Some(thing) => {
            ClientApi.thingInfo(thing, rc)
          }
          case None => throw new Exception("INTERNAL ERROR: Space claimed to create a new Thing, but seems to have failed?!?")
        }
      }
      case ThingError(error, stateOpt) => throw error
    }
  }
  
  private def getOnePropEditor(thing:Thing, prop:AnyProp, propVal:DisplayPropVal):Future[PropEditInfo] = {
    implicit val s = state
    val context = thing.thisAsContext(rc, state, ecology)
    for {
      rendered <- HtmlRenderer.renderPropertyInputStr(context, prop, propVal)
      prompt <- futOpt(prop.getPropOpt(Editor.PromptProp).filter(!_.isEmpty).map(_.renderPlain))
      summary <- futOpt(prop.getPropOpt(Conventions.PropSummary).map(_.render(prop.thisAsContext(rc, state, ecology))))
    }
      yield PropEditInfo(
        ClientApi.propInfo(prop, rc),
        propVal.inputControlId,
        prompt,
        summary,
        propVal.inheritedFrom.map(_.displayName),
        AccessControl.canEdit(state, user, prop),
        rendered
        )
  }
  
  def getOnePropertyEditor(thingId:TID, propId:TID):Future[PropEditInfo] = withThing(thingId) { thing =>
    implicit val s = state
    val result = for {
      prop <- state.prop(propId.toThingId)
      pv = thing.getPropOpt(prop)
      v = pv.map(_.v)
      dpv = DisplayPropVal(Some(thing), prop, v)
    }
      yield getOnePropEditor(thing, prop, dpv)
    
    // TODO: produce a more useful Exception if this fails:
    result.get
  }
  
  def getPropertyEditors(thingId:TID):Future[FullEditInfo] = withThing(thingId) { thing =>
    // Properties are very different from ordinary Things:
    thing match {
      case prop:AnyProp => getPropPropertyEditors(prop)
      case _ => getThingPropertyEditors(thing)
    }
  }
  
  private def getThingPropertyEditors(thing:Thing) = {
    implicit val s = state
    val model = thing.getModel
    
    val deriveNameOpt =
      for {
        pv <- thing.getPropOpt(DeriveName.DeriveNameProp)
        deriveLink <- pv.firstOpt
      }
        yield deriveLink == DeriveName.DeriveAlways.id
    val deriveName = deriveNameOpt.getOrElse(false)
    
    // If we're not auto-deriving the name, tell the PropListManager to add it in.
    // (Yes, this is badly coupled, and could probably use some rethinking.)
    val props = PropListManager.from(thing, !deriveName)
    val propList = 
      PropListManager.prepPropList(
          props, 
          Some(thing), 
          model, state, true)
          
    val filtered =
      if (deriveName)
        Editor.filteredPropIds + Core.NameProp.id
      else
        Editor.filteredPropIds
    val filteredPropList = 
      propList
      .filterNot(pair => filtered.contains(pair._1.id))
      .filterNot(pair => pair._1.ifSet(Core.InternalProp))
    val propInfoFuts = filteredPropList.map { entry =>
      val (prop, propVal) = entry
      getOnePropEditor(thing, prop, propVal)
    }
    val propInfos = Future.sequence(propInfoFuts)
    
    val instanceProps:Option[Seq[TID]] = for {
      instancePropsPair <- propList.find(_._1.id == querki.editing.MOIDs.InstanceEditPropsOID)
      instancePropsQV <- instancePropsPair._2.v
      instanceProps = instancePropsQV.rawList(Core.LinkType)
    }
      yield instanceProps.map(oid => TID(oid.toThingId))
      
    val instancePropsPath = new FieldIds(Some(thing), Editor.InstanceProps).inputControlId
    
    propInfos.map(pi => FullEditInfo(instanceProps.getOrElse(Seq.empty), instancePropsPath, deriveName, pi))
  }
  
  // TBD: this is pretty incestuous with the PropertyEditor in the client -- the list of available
  // props is defined here, in order. Not sure that's appropriate. Think about it...
  // TODO: someday, we should figure out what it means for a Property to have a Model. For now,
  // we're assuming that just doesn't happen.
  private def getPropPropertyEditors(prop:AnyProp):Future[FullEditInfo] = {
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
    val specialPropIds = Seq(Core.NameProp, Conventions.PropSummary, Conventions.PropDetails, Editor.EditWidthProp).map(_.id)
    val invariantPropIds = Seq(Core.TypeProp, Core.CollectionProp).map(_.id)
    val existingPropIds = ((prop.props.keys.toSet -- specialPropIds) -- invariantPropIds).toSeq
    val metaPropIds = (allMetapropsForTypeIds -- existingPropIds).toSeq
    
    // Specials are in their particular order; the rest get sorted by display:
    val specialProps = specialPropIds.map(onePropPair(_))
    val existingProps = existingPropIds.map(onePropPair(_))
    val metaProps = metaPropIds.map(onePropPair(_))
    val allProps = specialProps ++ (existingProps ++ metaProps).sortBy(_._2.prop.displayName)
    
    val allEditors = Future.sequence(allProps.map(entry => getOnePropEditor(prop, entry._1, entry._2)))
    // The Instance Property fields are meaningless for a Property:
    allEditors.map(FullEditInfo(Seq.empty, "", false, _))
  }
  
  def addPropertyAndGetEditor(thingId:TID, propIdStr:TID):Future[PropEditInfo] = withThing(thingId) { thing =>
    implicit val s = state
    val propId = propIdStr.toThingId
    val propsOpt = for {
      prop <- state.prop(propId)
      newV = prop.default
    }
      yield Core.toProps((prop, newV))

    self.request(createSelfRequest(ChangeProps2(thing.toThingId, propsOpt.get))) flatMap {
      case ThingFound(id, newState) => {
        val result = for {
          newThing <- newState.anything(id)
          prop <- newState.prop(propId)
          pv <- newThing.getPropOpt(prop)
          propVal = DisplayPropVal(Some(newThing), prop, Some(pv.v))
        }
          yield getOnePropEditor(newThing, prop, propVal)
        
        result.get
      }
    
      case ThingError(ex, _) => throw ex
    }        
  }
  
  def removeProperty(thingId:TID, propIdStr:TID):Future[PropertyChangeResponse] = withThing(thingId) { thing =>
    implicit val s = state
    val propId = propIdStr.toThingId
    val propsOpt = for {
      prop <- state.prop(propId)
      newV = DataModel.getDeletedValue(prop)
    }
      yield Core.toProps((prop, newV))

    propsOpt match {
      case Some(props) => doChangeProps(thing, props)
      case _ => Future.failed(querki.api.GeneralChangeFailure(s"Couldn't find Property $propId!"))
    }
  }
  
  def getModelType(modelId:TID):Future[TypeInfo] = withThing(modelId) { model =>
    implicit val s = state
    val typOpt = state.types.values.find { typ =>
      typ match {
        case mt:querki.types.ModelTypeBase => mt.basedOn == model.id
        case _ => false
      }
    }
    
    def toTypeInfo(t:PType[_]) = TypeInfo(t, t.linkName, t.displayName)
    
    typOpt match {
      case Some(typ) => Future.successful(toTypeInfo(typ))
      case None => {
        val props = Map(
            Types.ModelForTypeProp(model),
            Basic.DisplayNameProp("__" + model.displayName + " Type"))
            
        // TODO: note that there is technically a slight race condition here -- between us fetching the SpaceState
        // in withThing(), and us sending this message, somebody else could create the new Type. That's not
        // devastating, but it *is* unintentional, and it's a good example of why we need to move towards a more
        // transactional view of things, where our CreateThing message includes the version stamp of the state it
        // is based on, and fails if the stamp is out of date.
        val spaceMsg = CreateThing(rc.requesterOrAnon, state.id, Kind.Type, Core.UrType, props)
        
        spaceRouter.request(spaceMsg) map {
          case ThingFound(typeId, newState) => {
            val typ = newState.typ(typeId)
            toTypeInfo(typ)
          }
          case ThingError(error, stateOpt) => throw error
        }
      }
    }
  }

  def changeModel(thingId:TID, newModelId:TID):Future[ThingInfo] = withThing(thingId) { thing =>
    implicit val s = state
    state.anything(newModelId.toThingId) match {
      case Some(newModel) => {
        // The UI should be checking this, but belt and suspenders:
        if (!AccessControl.canCreate(state, user, newModel.id))
          throw new Exception(s"You aren't allowed to create a $newModelId!")
        
        // TODO: in principle, this should route through the UserSpaceSession. It doesn't matter yet, but is
        // likely to once we put Experiment Mode into place.
        // TODO: switch this to a separate message, tuned to changing model, and stop overloading this!
        val spaceMsg = ModifyThing(user, state.id, thing.id.toThingId, newModel.id, thing.props)
        spaceRouter.request(spaceMsg) flatMap {
          case ThingFound(newThingId, newState) => {
            newState.anything(newThingId) match {
              case Some(newThing) => ClientApi.thingInfo(newThing, rc)
              case None => throw new Exception(s"Change Model somehow resulted in unknown Thing $newThingId!")
            }
          }
          case ThingError(error, stateOpt) => throw error
        }          
      }
      case None => Future.failed(new Exception(s"Unknown model $newModelId!")) 
    }
  }
  
  // TODO: is this entry point needed any more?
  def getUndefinedTagView(modelId:TID):String = withThing(modelId) { model =>
    Tags.getUndefinedTagView(model.id)(state).text
  }
  
  def getPropertyUsage(propTid:TID):PropUsage = withProp(propTid) { prop =>
    implicit val s = state
    val propId = prop.id
    val uses = state.everythingLocal.filter(_.props.contains(propId))
    val (models, instances) = uses.partition(_.isModel)
    PropUsage(models.size, instances.size)
  }
  
  def removePropertyFromAll(propTid:TID):OperationHandle = withProp(propTid) { prop =>
    // All the work gets done by the child Actor:
    ProgressActor.createProgressActor(requester, RemovePropertyActor.props(user, prop.id, ecology, state, spaceRouter))
  }
}
