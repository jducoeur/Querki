package controllers

import language.existentials

import scala.concurrent.Future
import scala.util._

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.libs.concurrent.Promise
import play.api.mvc._
import models._
import Property._

import querki.core.{NameUtils, PropList, QLText}
import querki.ecology._
import querki.identity._

import querki.html.HtmlRenderer
import querki.session.messages._
import querki.spaces.messages._
import SpaceError._

import querki.util._
import querki.values._

class Application extends ApplicationBase {
  
  val newSpaceForm = Form(
    mapping(
      "name" -> nonEmptyText
    )((name) => name)
     ((name:String) => Some(name))
  )
  
  case class NewThingForm(model:String)
  val newThingForm = Form(
    mapping(
      "model" -> text
    )((model) => NewThingForm(model))
     ((info:NewThingForm) => Some((info.model)))
  )
  
  val searchForm = Form(
    mapping(
      "searchInput" -> nonEmptyText
    )((searchInput) => searchInput)
     ((searchInput:String) => Some(searchInput))
  )
  
  lazy val Core = interface[querki.core.Core]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val DataModel = interface[querki.datamodel.DataModelAccess]
  lazy val DeriveName = interface[querki.types.DeriveName]
  lazy val Editor = interface[querki.editing.Editor]
  lazy val HtmlRenderer = interface[querki.html.HtmlRenderer]
  lazy val Imexport = interface[querki.imexport.Imexport]
  lazy val Links = interface[querki.links.Links]
  lazy val Logic = interface[querki.logic.Logic]
  lazy val PropListMgr = interface[querki.core.PropListManager]
  lazy val QL = interface[querki.ql.QL]
  lazy val Search = interface[querki.search.Search]
  lazy val System = interface[querki.system.System]
  lazy val Tags = interface[querki.tags.Tags]
  lazy val Types = interface[querki.types.Types]
  
  lazy val AppliesToKindProp = Core.AppliesToKindProp
  lazy val DisplayNameProp = Basic.DisplayNameProp
  lazy val UrProp = Core.UrProp  
  
  def testHello = Action {
    Ok(views.html.testHello())
  }

  def index = withUser(false) { rc =>
    rc.requester match {
      case Some(requester) => {
	    askSpaceMgr[ListMySpacesResponse](ListMySpaces(requester.id)) { 
	      case MySpaces(mySpaces, memberOf) => Ok(views.html.spaces(rc, mySpaces, memberOf))
	    }        
      }
      case _ => Ok(views.html.index(rc))
    }
 }    
  
  // TODO: in the long run, we will want spidering of the main system pages, and allow users to
  // choose to have their pages indexed. We're quite a ways from that, though.
  def robots = Action { implicit request =>
    Ok("""# For the time being, Querki requests that robots stay out. This will change eventually.
        
user-agent: *
disallow: /
""").as("text/plain")
  }

  def spaces = withUser(true) { rc => 
    askSpaceMgr[ListMySpacesResponse](ListMySpaces(rc.requester.get.id)) { 
      case MySpaces(mySpaces, memberOf) => Ok(views.html.spaces(rc, mySpaces, memberOf))
    }
  }
    
  def space(ownerId:String, spaceId:String) = withSpace(false, ownerId, spaceId) { implicit rc =>
    // This entry point is deprecated -- we're moving to always look at the Space Thing itself, since
    // there are functions that only appear that way:
    Redirect(routes.Application.thing(ownerId, spaceId, spaceId))
  }
  
  def thing(ownerId:String, spaceId:String, thingId:String) = withThing(false, ownerId, spaceId, thingId, Some({ 
    case (ThingError(error, stateOpt), rc) if (error.msgName == UnknownName) => {
      // We didn't find the requested Thing, so display a TagThing for it instead:
      val rcWithName = rc.copy(thing = Some(Tags.getTag(thingId, stateOpt.get)))
      Ok(views.html.thing(rcWithName))
    }
  })) { implicit rc =>
    // Uncomment this to see details of the Thing we're displaying:
    //QLog.spewThing(rc.thing.getOrElse(rc.state.get))
    // rc now has all the interesting information copied into it:
    Ok(views.html.thing(rc))
  }
  
  def newSpace = withUser(true) { implicit rc =>
    if (rc.requesterOrAnon.canOwnSpaces) {
      Ok(views.html.newSpace(rc))
    } else {
      // TODO: internationalize this error message
      doError(routes.Application.index, "You aren't yet allowed to create Spaces")
    }
  }
  
  def doNewSpace = withUser(true) { implicit rc =>
    implicit val request = rc.request
    val requester = rc.requester.get
    newSpaceForm.bindFromRequest.fold(
      errors => doError(routes.Application.newSpace, "You have to specify a legal space name"),
      name => {
        TryTrans[Unit, Future[Result]] { Core.NameProp.validate(name, System.State) }.
          onSucc { _ =>
            askSpaceMgr[ThingResponse](CreateSpace(requester, name)) {
              case ThingFound(_, state) => Redirect(routes.Application.thing(requester.mainIdentity.handle, state.toThingId, state.toThingId))
              case ThingError(ex, _) => doError(routes.Application.newSpace, ex)
            }
          }.
          onFail { doError(routes.Application.newSpace, _) }.
          result
      }
    )
  }

  def otherModels(state:SpaceState, mainModel:Thing):Iterable[Thing] = {
    mainModel.kind match {
      case Kind.Thing | Kind.Attachment => state.allModels
      // For the time being, we're only allowing basic Properties. We might allow Properties to
      // serve as Models, but one thing at a time.
      case Kind.Property => Seq(UrProp)
      case Kind.Space => Seq(System.State)
      case _ => throw new Exception("Don't yet know how to create a Thing of kind " + mainModel.kind)
    }
  }
  
  def showEditPage(rc: PlayRequestContext, thingOpt:Option[Thing], model:Thing, props:PropList, errorMsg:Option[String] = None) = {
    implicit val state = rc.state.get
    val propList = PropListMgr.prepPropList(props, thingOpt, model, rc.state.get).
      // If the name is being derived, don't show it in the Editor UI:
      filter(DeriveName.filterNameIfDerived(state, model, props, _))
    try { 
      val page = views.html.editThing(
        rc.copy(error = errorMsg),
        model,
        otherModels(state, model),
        propList
      )
      if (errorMsg.isDefined)
        BadRequest(page)
      else
        Ok(page)    
    } catch {
      case e:Error => Logger.error("Error while displaying Editor: " + e)
      // TODO: put a real error message here:
      BadRequest("Internal error!")
    }
  }
  
  def createThing(ownerId:String, spaceId:String, modelIdOpt:Option[String]) = withSpace(true, ownerId, spaceId) { implicit rc =>
    implicit val state = rc.state.get
    val modelThingIdOpt = modelIdOpt map (ThingId(_))
    val modelOpt = modelThingIdOpt flatMap (rc.state.get.anything(_))
    val model = modelOpt getOrElse Basic.SimpleThing
    val createModel = rc.isTrue("asModel")
    val inherited = PropListMgr.inheritedProps(None, model)
    val allProps =
      if (createModel) {
        inherited + (Core.IsModelProp -> DisplayPropVal(None, Core.IsModelProp, Some(Core.ExactlyOne(Logic.True))))
      } else
        inherited
    showEditPage(rc, None, model, allProps)
  }
  
  def createProperty(ownerId:String, spaceId:String) = withSpace(true, ownerId, spaceId) { implicit rc =>
    showEditPage(
        rc, 
        None,
        UrProp,
        PropListMgr.inheritedProps(None, UrProp)(rc.state.get))
  }
  
  def doCreateThing(ownerId:String, spaceId:String, subCreate:Option[Boolean]) = {
    editThingInternal(ownerId, spaceId, None, false)
  }
  
  /**
   * Alternate create route -- this creates the new Thing, expecting fields to mainly be empty, then displays
   * the Thing Editor to edit it.
   * 
   * TODO: this is currently referred to in a lot of places. Can we expose a single entry point to compute this
   * instead? Might be cleaner.
   */
  def doCreateThing2(ownerId:String, spaceId:String, model:String) = {
    editThingInternal(ownerId, spaceId, None, false, Some({ (state, model, thingId, rc) =>
      implicit val implRc = rc
      state.anything(thingId) match {
        case Some(thing) => {
	      val context = thing.thisAsContext
	      val result = QL.process(QLText(s"""### New ${model.displayName}
	          |
	          |[[_edit]]""".stripMargin), context, None, Some(thing)).display.html
	      Ok(views.html.main(QuerkiTemplate.Edit, "Creating a new " + model.displayName, rc)(result))
        }
        case None => NotAcceptable("Unable to create " + model.displayName)
      }
    }))
  }
  
  lazy val doLogEdits = Config.getBoolean("querki.test.logEdits", false)
  
  def editThingInternal(ownerId:String, spaceId:String, thingIdStr:Option[String], partial:Boolean, 
      successCb:Option[(SpaceState, Thing, OID, PlayRequestContext) => Result] = None,
      errorCb:Option[(String, Thing, List[FormFieldInfo]) => Result] = None) = 
  withSpace(true, ownerId, spaceId, thingIdStr) { implicit rc =>
    implicit val request = rc.request
    implicit val state = rc.state.get
    val user = rc.requester.get
    val rawForm = newThingForm.bindFromRequest
    
    rawForm.fold(
      // TODO: What can cause this?
      errors => doError(routes.Application.space(ownerId, spaceId), "Something went wrong"),
      info => {
        val context = QLRequestContext(rc)
    
        // Whether we're creating or editing depends on whether thing is specified:
        val thing = rc.thing
        
        // Go through all of the form fields that we got from the client, and extract the actual
        // properties there based on their names:
        val fieldIds:List[FieldIds] = rawForm.data.keys.map(key => DisplayPropVal.propPathFromName(key, thing)).flatten.toList.sortBy(_.fullPropId)
        
        if (doLogEdits) {
          QLog.spew("Raw form fields:")
          rawForm.data.foreach(pair => QLog.spew(s"    ${pair._1}: ${pair._2}"))
          QLog.spew("FieldIds:")
          fieldIds.foreach(fieldId => QLog.spew(s"    ${fieldId.fullPropId}"))
          QLog.spew("Processing:")
        }

        // Since rebuildBundle can result in changes that build on each other, we need a thing that
        // responds to those changes.
        // TODO: we might want to redo this more properly functional, instead of relying on a var:
        var updatingThing = thing
        // HACK: if we are creating a new Thing, we need *something* for rebuildBundle to work on! I think this
        // suggests that this algorithm is well and truly fucked up at this point.
        var createBundle:ThingState = ThingState(UnknownOID, UnknownOID, UnknownOID, () => Map.empty)
        val rawProps:List[FormFieldInfo] = fieldIds map { fieldId =>
          if (doLogEdits) QLog.spew(s"  $fieldId...")
          val higherFieldIdsOpt = fieldId.container
          val actualFormFieldInfo = HtmlRenderer.propValFromUser(fieldId, updatingThing, rawForm, context)
          if (doLogEdits) QLog.spew(s"    actual: $actualFormFieldInfo")
          val result = {
            higherFieldIdsOpt match {
              case Some(higherFieldIds) => {
                // TEMP: restructure rebuildBundle to take higherFieldIds directly:
                def toHigherIds(oneFieldId:FieldIds):List[IndexedOID] = {
                  val thisId = IndexedOID(oneFieldId.p.id, oneFieldId.index)
                  oneFieldId.container match {
                    case Some(c) => toHigherIds(c) ::: List(thisId)
                    case None => List(thisId)
                  }
                }
                Types.rebuildBundle(updatingThing orElse Some(createBundle), toHigherIds(higherFieldIds), actualFormFieldInfo).
                  getOrElse(FormFieldInfo(UrProp, None, true, false, None, Some(new PublicException("Didn't get bundle"))))         
              }
              case None => actualFormFieldInfo
            }
          }
          if (doLogEdits) QLog.spew(s"    result: $result")
          updatingThing match {
            case None if (result.isValid && !result.isEmpty) => {
              val newProps = createBundle.props + (result.propId -> result.value.get)
              createBundle = createBundle.copy(pf = () => newProps)              
            }
            case Some(ts @ ThingState(_, _, _, _, _, _)) if (result.isValid && !result.isEmpty) => {
              val newProps = ts.props + (result.propId -> result.value.get)
              updatingThing = Some(ts.copy(pf = () => newProps))
            }
            case _ => {
              // We really ought to cope with other Kinds here, at least in principle...
            }
          }
          result
        }
        val oldModel =
          if (info.model.length() > 0)
            state.anything(OID(info.model)).get
          else
            thing.map(_.getModel).getOrElse(throw new Exception("Trying to edit thing, but we don't know the model!"))
        
        val kind = oldModel.kind
        
        def formFlag(fieldName:String):Boolean = {
          rawForm(fieldName).value.map(_ == "true").getOrElse(false)
        }
        
        val makeAnother = formFlag("makeAnother")        
        val fromAPI = formFlag("API")
        
        def makeProps(propList:List[FormFieldInfo]):PropList = {
          val modelProps = PropListMgr.inheritedProps(thing, oldModel)
          val nonEmpty = propList filterNot (_.isEmpty)
          // Iff there are props whose submitted values are invalid, set them back to the
          // previous value, or the default if there wasn't one.
          val repaired = propList filterNot (_.isValid) map { info =>
            val prop = info.prop
            val oldValOpt = thing.flatMap(_.localPropVal(prop))
            oldValOpt match {
              case Some(v) => FormFieldInfo(prop, Some(v), false, true)
              case None => FormFieldInfo(prop, Some(prop.default), false, true)
            }
          }
          (modelProps /: (nonEmpty ++ repaired)) { (m, fieldInfo) =>
            val prop = fieldInfo.prop
            val disp =
              if (m.contains(prop))
                m(prop).copy(v = fieldInfo.value)
              else
                DisplayPropVal(thing, prop, fieldInfo.value)
            m + (prop -> disp)              
          }
        }
        
        val redisplayStr = rawForm("redisplay").value.getOrElse("")
        
        if (redisplayStr.length() > 0 && redisplayStr.toBoolean) {
          showEditPage(rc, updatingThing, oldModel, makeProps(rawProps))
//        } else if (info.newModel.length > 0) {
//          // User is changing models. Replace the empty Properties with ones
//          // appropriate to the new model, and continue:
//          // TODO: for now, there is no way to get to here -- it's an annoying edge case,
//          // and mucking up the UI. Later, let's reintroduce a less-intrusive way to
//          // invoke this.
//          val model = state.anything(OID(info.newModel)).get
//          showEditPage(rc, model, makeProps(rawProps))
        } else {
          // User has submitted a creation/change. Is it legal?
          val illegalVals = rawProps.filterNot(_.isValid)
          if (illegalVals.isEmpty) {
            val filledProps = rawProps.filterNot(_.isEmpty)
            val emptyProps = rawProps.filter(_.isEmpty)
            // Everything parses, anyway, so send it on to the Space for processing:
            val propPairs = filledProps.map { pair =>
              val FormFieldInfo(prop, value, _, _, _, _) = pair
              (prop.id, value.get)
            }
            val props = Core.toProps(propPairs:_*)()
            val spaceMsg = if (thing.isDefined) {
              if (partial || oldModel.hasProp(querki.editing.MOIDs.InstanceEditPropsOID)) {
                // Editing an instance, so we only have a subset of the props, not the full list.
                // NOTE: the reason this is separated from the next clause is because ChangeProps gives us
                // no way to *delete* a property. We don't normally expect to do that in a limited-edit instance,
                // but we certainly need to be able to in the general case.
                // TODO: refactor these two clauses together. ModifyThing is inappropriate when InstanceEditProps is
                // set, because it expects to be editing the *complete* list of properties, not just a subset.
                // Now that we can signal deletions in ChangeProps, and InstanceProps is becoming standard, ModifyThing may become vestigial...
                val deletions:Thing.PropMap = {
                  if (partial)
                    // If it's a partial, don't go messing with deletions -- probably only one property was specified, and
                    // it's intentional:
                    Map.empty
                  else updatingThing match {
                    // It's not a partial, but since there are Instance Properties at play, we have to dance around
                    // to figure out whether anything got cleared or deleted:
                    case Some(thing) => {
                      val locals = Editor.propsNotInModel(thing, state)
                      // First, we delete the local properties:
                      val localDeletions = (Map.empty[OID, QValue] /: locals) { (curMap, propId) =>
                        if (!props.contains(propId))
                          curMap + (propId -> DataModel.DeletedValue)
                        else
                          curMap
                      }
                      // Then, any inherited Properties that were defined but are now empty:
                      (localDeletions /: emptyProps) { (curMap, formFieldInfo) =>
                        if (thing.props.contains(formFieldInfo.propId))
                          curMap + (formFieldInfo.propId -> DataModel.DeletedValue)
                        else
                          curMap
                      }
                    }
                    case None => Map.empty
                  }
                }
                
                if (doLogEdits) {
                  QLog.spew("Deletions:")
                  deletions.foreach(deletion => QLog.spew(s"      ${deletion._1}"))
                }
                
                SessionRequest(user, rc.ownerId, ThingId(spaceId), ChangeProps2(thing.get.id.toThingId, props ++ deletions))
              } else {
                // Editing an existing Thing. If InstanceEditPropsOID isn't set, we're doing a full edit, and
                // expect to have received the full list of properties.
                ModifyThing(user, rc.ownerId, ThingId(spaceId), thing.get.id.toThingId, OID(info.model), props)
              }
            } else {
              // Creating a new Thing
              CreateThing(user, rc.ownerId, ThingId(spaceId), kind, OID(info.model), props)
            }
        
            askSpaceMgr[ThingResponse](spaceMsg) {
              case ThingFound(thingId, newState) => {
                // TODO: the default pathway described here really ought to be in the not-yet-used callback, and get it out of here.
                // Indeed, all of this should be refactored:
                val newRc = rc.copy(state = Some(newState))
                successCb.map(cb => cb(newState, oldModel, thingId, newRc)).map(fRes(_)).getOrElse {
                  val thing = newState.anything(thingId).get
                  
                  if (makeAnother)
                    showEditPage(newRc, Some(thing), oldModel, PropListMgr.inheritedProps(None, oldModel)(newState))
                  else if (rc.isTrue("subCreate")) {
                    Ok(views.html.subCreate(rc, thing));
                  } else if (fromAPI) {
                    // In the AJAX case, we just send back the OID of the Thing:
                    Ok(thingId.toThingId.toString)
                  } else {
                    Redirect(routes.Application.thing(newState.ownerHandle, newState.toThingId, thing.toThingId.encoded))
                  }
                }
              }
              case ThingError(error, stateOpt) => {
                val msg = error.display
                errorCb.map(cb => cb(msg, oldModel, rawProps)).map(fRes(_)).getOrElse {
                  if (fromAPI) {
                    NotAcceptable(msg)
                  } else {
                    showEditPage(rc, updatingThing, oldModel, makeProps(rawProps), Some(msg))
                  }
                }                
              }
            }
          } else {
            // One or more values didn't parse against their PTypes. Give an error and continue:
            val badProps = illegalVals.map { info => 
              info.prop.displayName + info.error.map(": " + _.display).getOrElse("")
            }
            val errorMsg = "Illegal values -- " + badProps.mkString(", ")
            errorCb.map(cb => cb(errorMsg, oldModel, rawProps)).map(fRes(_)).getOrElse {
              if (fromAPI) {
                NotAcceptable(errorMsg)
              } else {
                showEditPage(rc, updatingThing, oldModel, makeProps(rawProps), Some(errorMsg))
              }
            }
          }
        }
      }      
    )
  }

  def editThing(ownerId:String, spaceId:String, thingIdStr:String) = withSpace(true, ownerId, spaceId, Some(thingIdStr), Some({ 
    // TODO: can/should we refactor this out to a new Tags Module?
    case (ThingError(error, stateOpt), rc) if (error.msgName == UnknownName) => {
      // We didn't find the requested Thing, so we're essentially doing a variant of Create instead.
      // This happens normally when you "Edit" a Tag.
      implicit val state = rc.state.get
      // If this Tag is used in a Tag Set Property with a Link Model, use that:
      val model = Tags.preferredModelForTag(state, thingIdStr)
      val name = NameUtils.toDisplay(thingIdStr)
      val defaultText =
        model.getPropOpt(Tags.ShowUnknownProp).orElse(state.getPropOpt(Tags.ShowUnknownProp)).
          map(_.v).
          getOrElse(Core.ExactlyOne(Core.LargeTextType(querki.tags.defaultDisplayText)))
      showEditPage(rc, None, model, 
          PropListMgr.inheritedProps(None, model) ++
          PropListMgr(DisplayNameProp -> DisplayPropVal(None, DisplayNameProp, Some(Core.ExactlyOne(Basic.PlainTextType(name)))),
                   (Basic.DisplayTextProp -> DisplayPropVal(None, Basic.DisplayTextProp, Some(defaultText)))))
    }
  })) { implicit rc =>
    implicit val state = rc.state.get
    val thing = rc.thing.get
    // TODO: security check that I'm allowed to edit this
	val model = thing.getModel
	showEditPage(rc, Some(thing), model, PropListMgr.from(thing))
  }
  
  def editInstances(ownerId:String, spaceId:String, modelIdStr:String) = withThing(true, ownerId, spaceId, modelIdStr) { implicit rc =>
    implicit val state = rc.state
    val thing = rc.thing.get
    val editText = QLText(s"""### Editing instances of ____
        |
        |[[_edit]]
        |
        |[[_linkButton(""Done"")]]""".stripMargin)
    val wikitext = QL.process(editText, thing.thisAsContext, None, Some(thing))
    val html = wikitext.display.html
    Ok(views.html.main(QuerkiTemplate.Thing, s"Editing instances of ${thing.displayName}", rc, true)(html))
  }
  
  def doEditThing(ownerId:String, spaceId:String, thingIdStr:String) = editThingInternal(ownerId, spaceId, Some(thingIdStr), false)
  
  def changeModel(ownerId:String, spaceId:String, thingIdStr:String) = withThing(true, ownerId, spaceId, thingIdStr) { implicit rc =>
    val state = rc.state.get
    val thing = rc.thing.get
    val modelIdOpt = rc.firstQueryParam("modelId")
    modelIdOpt.flatMap(modelIdStr => state.anything(ThingId(modelIdStr))) match {
      case Some(model) => {
        val spaceMsg = ModifyThing(rc.requesterOrAnon, rc.ownerId, ThingId(spaceId), rc.thing.get.id.toThingId, model.id, thing.props)
        askSpaceMgr[ThingResponse](spaceMsg) {
          case ThingFound(thingId, newState) => {
            val newThing = newState.anything(thingId)
            Redirect(routes.Application.thing(newState.ownerHandle, newState.toThingId, newThing.get.toThingId))
          }
          case ThingError(error, stateOpt) => {
        	doError(routes.Application.thing(ownerId, spaceId, thingIdStr), error)
          }
        }
      }
      case None => doError(routes.Application.thing(ownerId, spaceId, thingIdStr), "Unknown Model: " + modelIdOpt)
    }
  }

  // TODO: this should really have its own security property, Can View Source, which should default to Can Read. But for now,
  // we'll make do with Can Read, which is implicit in withThing:
  def viewThing(ownerId:String, spaceId:String, thingIdStr:String) = withThing(false, ownerId, spaceId, thingIdStr) { implicit rc =>
    Ok(views.html.viewSource(rc))
  }
  
  def deleteThing(ownerId:String, spaceId:String, thingId:String) = withThing(true, ownerId, spaceId, thingId) { implicit rc =>
    val thing = rc.thing.get
    val displayName = thing.displayName
    val deleteMsg = DeleteThing(rc.requester.get, rc.ownerId, rc.state.get.toThingId, thing.toThingId)
    askSpaceMgr[ThingResponse](deleteMsg) {
      case ThingFound(thingId, newState) => {
        if (rc.APICall) {
          Ok("Deleted")
        } else {
          Redirect(routes.Application.space(ownerId, spaceId)).flashing("info" -> (displayName + " deleted."))
        }
      }
      case ThingError(error, stateOpt) => {
        if (rc.APICall) {
          InternalServerError(error.display(Some(rc)))
        } else {
          doError(routes.Application.space(ownerId, spaceId), error)
        }
      }
    }
  }
  
  def search(ownerId:String, spaceId:String) = withSpace(false, ownerId, spaceId) { implicit rc =>
    implicit val request = rc.request
    searchForm.bindFromRequest.fold(
      errors => { doError(routes.Application.space(ownerId, spaceId), "That wasn't a legal search") },
      searchInput => {
        import querki.search._
        val resultsOpt = Search.search(rc, searchInput)
        resultsOpt.map(results => Ok(views.html.searchResults(rc, results))).map(fRes(_)).getOrElse(doError(routes.Application.space(ownerId, spaceId), "That wasn't a legal search"))
      }
    )
  }

  /**
   * This is essentially the interactive version of doEditThing. It expects to be called from AJAX, with a form that
   * has been constructed to look like the traditional Editor window.
   */
  def setProperty2(ownerId:String, spaceId:String, thingId:String) = {
    editThingInternal(ownerId, spaceId, Some(thingId), true)
  }
  
  /**
   * Fetch the value of a single property, in HTML-displayable form, via AJAX.
   * 
   * This is used by the Editor to get the Default View for properties, but is likely to be broadly useful in the long run.
   * 
   * Note that canRead is applied automatically for the Thing, up in withSpace().
   */
  def getPropertyDisplay(ownerId:String, spaceId:String, thingId:String, prop:String) = withThing(true, ownerId, spaceId, thingId) { implicit rc =>
    val resultOpt = for (
      thing <- rc.thing;
      prop <- rc.prop;
      wikitext = thing.render(rc, Some(prop))
        )
      yield wikitext.display.toString
    
    Ok(resultOpt.getOrElse("Couldn't find that property value!"))
  }
  
  /**
   * Fetch the standard property-editor HTML for the specified property.
   */
  def getPropertyEditor(ownerId:String, spaceId:String, thingId:String, prop:String, i:Int) = withSpace(true, ownerId, spaceId, if (thingId.length() > 0) Some(thingId) else None) 
  { implicit rc =>
    val resultOpt = for {
      prop <- rc.prop;
      propVal = DisplayPropVal(rc.thing, prop, None)
    }
      yield views.html.showPropertyTemplate(rc, prop, propVal, i).toString.trim()
      
    Ok(resultOpt.getOrElse("Couldn't create that property editor!"))
  }
  
  /**
   * AJAX: Fetch the "live editor" for the specified Thing.
   */
  def getThingEditor(ownerId:String, spaceId:String, thingId:String) = withThing(true, ownerId, spaceId, thingId) { implicit rc =>
    val resultOpt = 
      for (
        thing <- rc.thing;
        wikitext = Editor.getInstanceEditor(thing, QLRequestContext(rc))
      )
        yield wikitext.display.toString
        
    Ok(resultOpt.getOrElse("Couldn't find that Thing!"))
  }

  /**
   * AJAX call to fetch the existing tag values for the specified property.
   */
  def getTags(ownerId:String, spaceId:String, propId:String, q:String) = withSpace(true, ownerId, spaceId) { implicit rc =>
    implicit val space = rc.state.get
    val lowerQ = q.toLowerCase()
    val propOpt = space.prop(ThingId(propId))
    val tagsOpt = for
      (
        prop <- propOpt
      )
        yield Tags.fetchTags(space, prop).filter(_.toLowerCase().contains(lowerQ))
        
    val tagsSorted = tagsOpt.map(tags => tags.toList.sorted)
    val thingsSorted = propOpt.map(prop => getLinksFromSpace(space, propOpt, lowerQ)).getOrElse(Seq.empty).map(_._1)
        
    val tagsAndThings = 
      tagsSorted match {
        case Some(tags) => tags ++ (thingsSorted.toList.diff(tags))
        case None => thingsSorted
      }
        
    // TODO: introduce better JSONification for the AJAX code:
    val JSONtags = "[" + tagsAndThings.map(name => "{\"display\":\"" + name + "\", \"id\":\"" + name + "\"}").mkString(",") + "]"
    Ok(JSONtags)
  }
  
  def getLinksFromSpace(spaceIn:SpaceState, propOpt:Option[Property[_,_]], lowerQ:String):Seq[(String,OID)] = {
    implicit val space = spaceIn
    
    val thingsSorted = {
      val allThings = space.allThings.toSeq
    
      // Filter the options if there is a valid Link Model:
      val thingsFiltered = {
        val filteredOpt = for (
          prop <- propOpt;
          linkModelProp <- prop.getPropOpt(Links.LinkModelProp);
          targetModel <- linkModelProp.firstOpt
            )
          yield allThings.filter(_.isAncestor(targetModel))
          
        filteredOpt.getOrElse(allThings)
      }
    
      thingsFiltered.map(t => (t.displayName, t.id)).filter(_._1.toLowerCase().contains(lowerQ)).sortBy(_._1)
    }
    
    space.app match {
      case Some(app) => thingsSorted ++ getLinksFromSpace(app, propOpt, lowerQ)
      case None => thingsSorted
    }
  }

  def getLinks(ownerId:String, spaceId:String, propId:String, q:String) = withSpace(true, ownerId, spaceId) { implicit rc =>
    val lowerQ = q.toLowerCase()
    val space = rc.state.get
    val propOpt = {
      if (propId.length() > 0)
        space.prop(ThingId(propId))
      else
        None
    }
    
    val results = getLinksFromSpace(space, propOpt, lowerQ)
    
    // TODO: introduce better JSONification for the AJAX code:
    val items = results.map(item => "{\"display\":\"" + item._1 + "\", \"id\":\"" + item._2 + "\"}")
    val JSONtags = "[" + items.mkString(",") + "]"
    Ok(JSONtags)
  }

  def upload(ownerId:String, spaceId:String) = withSpace(true, ownerId, spaceId) { implicit rc =>
    Ok(views.html.upload(rc))
  }

  // TODO: I think this function is the straw that breaks the camel's back when it comes to code structure.
  // It has too many nested levels, and they're only going to get worse. It needs a big rewrite.
  //
  // A better solution is probably to use 2.10's Try monad pretty rigorously. Say that askSpaceMgr returns
  // a monadic response, that composes by calling the next step in the chain. If you get ThingFound, we
  // continue; if you get ThingError, we fail the Try with an error. Done right, and all of this can
  // become a nice for() statement, and will be much easier to reason about in the long run. That will
  // have to wait until we have 2.10 going, though.
  //
  // TODO: generalize upload. You should be able to select the desired file type from a whitelist,
  // and upload that type properly. There should be type-specific validation -- for instance, CSS should
  // be Javascript-scrubbed. There should be type-specific post-processing -- for instance, photos
  // should automatically generate thumbnails.
  //
  // TODO: limit the size of the uploaded file!!!
  def doUpload(ownerId:String, spaceId:String) = withSpace(true, ownerId, spaceId) { implicit rc =>
    implicit val state = rc.state.get
    val user = rc.requester.get
    val body = rc.request.body match {
      case r:AnyContent => r
      case _ => throw new Exception("Somehow got a weird body content in doUpload!")
    }
    body.asMultipartFormData flatMap(_.file("uploadedFile")) map { filePart =>
      val filename = filePart.filename
	  val tempfile = filePart.ref.file
	  // TBD: Note that this codec forces everything to be treated as pure-binary. That's
	  // because we are trying to be consistent about attachments. Not clear whether
	  // that's the right approach in the long run, though.
	  val source = io.Source.fromFile(tempfile)(scala.io.Codec.ISO8859)
	  val contents = try {
	    // TODO: is there any way to get the damned file into the DB without copying it
	    // through memory like this?
	    source.map(_.toByte).toArray
	  } finally {
	    source.close()
	  }
	  val attachProps = Core.toProps(DisplayNameProp(filename))()
	  askSpaceMgr[ThingResponse](
	    CreateAttachment(user, rc.ownerId, state.id.toThingId, contents, MIMEType.JPEG, contents.size, querki.basic.MOIDs.PhotoBaseOID, attachProps)) {
	    case ThingFound(attachmentId, state2) => {
	      Redirect(routes.Application.thing(ownerId, state.toThingId, attachmentId.toThingId))
	    }
	    case ThingError(error, stateOpt) => {
          doError(routes.Application.upload(ownerId, spaceId), error)
	    }
	  }
	} getOrElse {
      doError(routes.Application.upload(ownerId, spaceId), "You didn't specify a file")
	}
  }

  // TODO: this is broken from a security POV. It should be rewritten in terms of GetSpace!
  def attachment(ownerIdStr:String, spaceId:String, thingIdStr:String) = withUser(false) { rc =>
    val ownerId = getIdentityByThingId(ownerIdStr)
    askSpaceMgr[SpaceResponse](GetAttachment(rc.requesterOrAnon, ownerId, ThingId(spaceId), ThingId(thingIdStr))) {
      case AttachmentContents(id, size, mime, content) => {
        Ok(content).as(mime)
      }
      // TODO: this should probably include the error message in some form? As it is, you get a blank page
      // if you try to download and it fails:
      case ThingError(err, _) => BadRequest
      case ThingFound(_, _) => QLog.error("Application.attachment somehow got a ThingFound back!"); BadRequest
    }     
  }
  
  /**
   * Given a Model, return the Type that wraps around that Model.
   * 
   * API ONLY.
   */
  def getModelType(ownerId:String, spaceId:String, modelId:String) = withThing(true, ownerId, spaceId, modelId) { rc =>
    implicit val state = rc.state.get
    val model = rc.thing.get
    val typOpt = state.types.values.find { typ =>
      typ match {
        case mt:querki.types.ModelTypeBase => mt.basedOn == model.id
        case _ => false
      }
    }
    
    typOpt match {
      case Some(typ) => {
        // We already have a Type for this Model:
        Ok(typ.id.toString)
      }
      case None => {
        val props = Map(
            Types.ModelForTypeProp(model),
            Basic.DisplayNameProp("__" + model.displayName + " Type"))
        // TODO: note that there is technically a slight race condition here -- between us fetching the SpaceState
        // in withThing(), and us sending this message, somebody else could create the new Type. That's not
        // devastating, but it *is* unintentional, and it's a good example of why we need to move towards a more
        // transactional view of things, where our CreateThing message includes the version stamp of the state it
        // is based on, and fails if the stamp is out of date.
        val spaceMsg = CreateThing(rc.requesterOrAnon, rc.ownerId, ThingId(spaceId), Kind.Type, Core.UrType, props)        
        askSpaceMgr[ThingResponse](spaceMsg) {
          case ThingFound(thingId, newState) => Ok(thingId.toString)
          case ThingError(error, stateOpt) => {
            val msg = error.display(rc.request)
            NotAcceptable(msg)
          }
        }        
      }
    }
  }
  
  def showAdvancedCommands(ownerId:String, spaceId:String, thingId:String) = withThing(true, ownerId, spaceId, thingId) { implicit rc =>
    implicit val state = rc.state.get
    val thing = rc.thing.get
    val editText = QLText(s"""### Advanced commands for ____
        |
        |**[Export all Instances of [[Name]] as a CSV file](_exportModel?modelId=[[_oid]]&format=1)**
        |
        |[[_if(_hasPermission(Can Read Comments._self), 
        |  ""**Send me a Message whenever someone comments in this Space:** [[_space -> _getCommentNotifications._edit]]
        |("Maybe" means the default: Yes if you are the owner of this space, No otherwise.)
        |**Note:** this may not take effect for a few hours."")]]
        |
        |[[_linkButton(""Done"")]]""".stripMargin)
    val wikitext = QL.process(editText, thing.thisAsContext, None, Some(thing))
    val html = wikitext.display.html
    Ok(views.html.main(QuerkiTemplate.Thing, s"Editing instances of ${thing.displayName}", rc, true)(html))    
  }
  
  def exportModel(ownerId:String, spaceId:String, modelId:String, format:Int) = withThing(true, ownerId, spaceId, modelId) { rc =>
    implicit val state = rc.state.get
    val model = rc.thing.get
    
    val result = Imexport.exportInstances(rc, format, model)
    
    Ok(result.content).as(result.mime).withHeaders(("Content-disposition" -> s"attachment;filename=${result.name}"))
  }
  
  def exportThing(ownerId:String, spaceId:String, thingIdStr:String) = withSpace(true, ownerId, spaceId, Some(thingIdStr)) { implicit rc =>
//    implicit val state = rc.state.get
//    val thing = rc.thing.get
//    // TODO: security check that I'm allowed to export this
//    val export = thing.export
//    Ok(export).as(MIMEType.JSON)
    Ok("NYI")
  }

  def sharing(ownerId:String, spaceId:String) = withSpace(true, ownerId, spaceId) { implicit rc =>
    if (rc.isOwner)
      Ok(views.html.sharing(rc))
    else
      doError(routes.Application.index, "Only the owner of the Space is currently allowed to manage its security")
  }

  /**
   * A tiny test function, to see if I understand how to use Ajax calls:
   */
  def testAjax(i1:String, i2:String) = Action { implicit request =>
    Logger.info(s"Got test values $i1 and $i2")
    try {
      val result = i1.toInt + i2.toInt
      Ok(result.toString)
    } catch {
      case e:Exception => Logger.info("Got an error while testing Ajax -- probably a badly formed number"); Ok("")
    }
  }
  
  def showTestAjax = withUser(false) { rc =>
    Ok(views.html.testAjax(rc))
  }
  
  def javascriptRoutes = Action { implicit request =>
    import routes.javascript._
    Ok(
      Routes.javascriptRouter("jsRoutes")(
        routes.javascript.Application.testAjax,
        routes.javascript.Application.setProperty2,
        routes.javascript.Application.getPropertyDisplay,
        routes.javascript.Application.getPropertyEditor,
        routes.javascript.Application.doCreateThing,
        routes.javascript.Application.getThingEditor,
        routes.javascript.Application.deleteThing,
        routes.javascript.Application.getModelType
      )
    ).as("text/javascript")
  }
}
