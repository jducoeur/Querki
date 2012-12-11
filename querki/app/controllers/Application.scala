package controllers

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.concurrent.Promise
import play.api.mvc._

import models._

import Property._

import system._
import models.system.SystemSpace._

object Application extends Controller {
  
  val userForm = Form(
    mapping(
      "name" -> nonEmptyText
    )((name) => User(UnknownOID, name))
     ((user: User) => Some((user.name)))
  )
  
  val newSpaceForm = Form(
    mapping(
      "name" -> nonEmptyText
    )((name) => name)
     ((name:String) => Some(name))
  )
  
  case class NewThingForm(values:List[String], fields:List[String], addedProperty:String, newModel:String, model:String)
  val newThingForm = Form(
    mapping(
      "value" -> list(text),
      "field" -> list(text),
      "addedProperty" -> text,
      "newModel" -> text,
      "model" -> text
    )((value, field, addedProperty, newModel, model) => NewThingForm(value, field, addedProperty, newModel, model))
     ((info:NewThingForm) => Some((info.values, info.fields, info.addedProperty, info.newModel, info.model)))
  )
  
  def getUser(username:String):Option[User] = User.get(username)
  
  def username(request: RequestHeader) = request.session.get(Security.username)

  def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Application.login)

  def withAuth(f: => String => Request[AnyContent] => Result) = {
    Security.Authenticated(username, onUnauthorized) { user =>
      Action(request => f(user)(request))
    }
  }
  
  def withUser(f: User => Request[AnyContent] => Result) = withAuth { username => implicit request =>
    getUser(username).map { user =>
      f(user)(request)
    }.getOrElse(onUnauthorized(request))
  }
  
  /**
   * Helper for asking the SpaceManager for info. Assumes that the process is
   * asynchronous, and buffers the incoming HTTP request accordingly.
   * 
   * @tparam A The type of the expected return message from SpaceManager. This usually names a trait;
   * the actual messages should be derived from that.
   * @param msg The message to send to the SpaceManager.
   * @param cb A partial function that takes the SpaceManager response and produces a result.
   */
  def askSpaceMgr[A](msg:SpaceMgrMsg)(cb: A => Result)(implicit m:Manifest[A]) = {
    Async {
      SpaceManager.ask[A, Result](msg)(cb)
    }
  }
  
  /**
   * Utility method for pages that are basically displaying some aspect of a whole Space.
   * 
   * @param spaceId The stringified OID of the space to display, from the URL
   * @param f A callback that, given the user and the space's state, produces the
   * actual page content
   */
  def withSpaceAndRequest(spaceId:String)(f: (User => SpaceState => Request[AnyContent] => Result)) = withUser { user => implicit request =>
    askSpaceMgr[GetSpaceResponse](GetSpace(OID(spaceId), user.id)) {
      case RequestedSpace(state) => f(user)(state)(request)
      case GetSpaceFailed(id, msg) => Ok(views.html.spaces(Some(user), Seq.empty, Some(msg)))
    }     
  }
  
  /**
   * Simpler version of withSpaceAndRequest, for the majority of methods that don't actually
   * care about the request.
   */
  def withSpace(spaceId:String)(f: (User => SpaceState => Result)) = withUser { user => implicit request =>
    askSpaceMgr[GetSpaceResponse](GetSpace(OID(spaceId), user.id)) {
      case RequestedSpace(state) => f(user)(state)
      case GetSpaceFailed(id, msg) => Ok(views.html.spaces(Some(user), Seq.empty, Some(msg)))
    }     
  }
  
  def index = Security.Authenticated(username, request => Ok(views.html.index(None, userForm))) { 
    user => Action {
      Ok(views.html.index(getUser(user), userForm)) 
    }
  }    

  def spaces = withUser { user => implicit request => 
    askSpaceMgr[ListMySpacesResponse](ListMySpaces(user.id)) { 
      case MySpaces(list) => Ok(views.html.spaces(Some(user), list))
    }
  }
    
  def space(spaceId:String) = withSpace(spaceId) { user => implicit state =>
    Ok(views.html.thing(Some(user), state))
  }
  
  def things(spaceId:String) = withSpace(spaceId) { user => implicit state =>
    Ok(views.html.things(Some(user)))
  }
  
  def thing(spaceId:String, thingId:String) = withSpace(spaceId) { user => implicit state =>
    Ok(views.html.thing(Some(user), state.anything(OID(thingId))))
  }
  
  def newSpace = withUser { user => implicit request =>
    Ok(views.html.newSpace(user))
  }
  
  def doNewSpace = withUser { user => implicit request =>
    newSpaceForm.bindFromRequest.fold(
      errors => BadRequest(views.html.newSpace(user, Some("You have to specify a legal space name"))),
      name => {
        if (NameProp.validate(name)) {
          askSpaceMgr[GetSpaceResponse](CreateSpace(user.id, name)) {
            case RequestedSpace(state) => Redirect(routes.Application.space(state.id.toString))
            // TODO: we'll want more granular failure messages:
            case GetSpaceFailed(id, msg) =>  BadRequest(views.html.newSpace(user, Some(msg)))
          }
        } else {
          BadRequest(views.html.newSpace(user, Some("That's not a legal Space name")))
        }
      }
    )
  }
  
  def getOtherProps(state:SpaceState, existing:PropList):Seq[Property[_,_,_]] = {
    val existingProps = existing.keys
    // TODO: sort alphabetically
    // TODO: filter out "non-user" Properties
    (state.allProps.values.toSet -- existingProps).toSeq
  }
  
  def replaceModelProps(existing:PropList, model:Thing)(implicit state:SpaceState):PropList = {
    val nonEmpty = existing.filter { keyval =>
      val current = keyval._2
      current.isDefined && current.get.length > 0
    }
    (nonEmpty /: model.allProps) { (m, prop) => 
      if (m.contains(prop)) m else m + (prop -> None)
    }
  }
  
  def showEditPage(user:User, existing:Option[Thing], model:Thing, props:PropList, errorMsg:Option[String] = None)(implicit state:SpaceState) = {
    val page = views.html.editThing(
        user, 
        existing,
        model,
        state.allModels,
        props,
        getOtherProps(state, props),
        errorMsg
      )
    if (errorMsg.isDefined)
      BadRequest(page)
    else
      Ok(page)    
  }
  
  def createThing(spaceId:String) = withSpace(spaceId) { user => implicit state =>
    showEditPage(user, None, SimpleThing, PropList((NameProp -> None)))
  }
  
  def doCreateThing(spaceId:String) = editThingInternal(spaceId:String, None)
  
  def editThingInternal(spaceId:String, thingIdStr:Option[String]) = withSpaceAndRequest(spaceId) { user => implicit state => implicit request =>
    newThingForm.bindFromRequest.fold(
      // TODO: correct error message here:
      errors => BadRequest(views.html.newSpace(user, Some("You have to specify a legal name"))),
      info => {
        // Whether we're creating or editing depends on whether thingIdStr is specified:
        val thingId = thingIdStr map (OID(_))
        val thing = thingId map (state.anything(_))
        val rawProps = info.fields.zip(info.values)
        val oldModel = state.anything(OID(info.model))
        
        def makeProps(propList:List[(String, String)]):PropList = {
          val rawList = propList.map { pair =>
            val (propIdStr, rawValue) = pair
            val propId = OID(propIdStr)
            val prop = state.prop(propId)
            (prop -> Some(rawValue))
          }
          PropList(rawList:_*)
        }
        
        if (info.addedProperty.length > 0) {
          // User chose to add a Property; add that to the UI and continue:
          val allProps = rawProps :+ (info.addedProperty, "")
          showEditPage(user, thing, oldModel, makeProps(allProps))
        } else if (info.newModel.length > 0) {
          // User is changing models. Replace the empty Properties with ones
          // appropriate to the new model, and continue:
          val model = state.anything(OID(info.newModel))
          showEditPage(user, thing, model, replaceModelProps(makeProps(rawProps), model))
        } else {
          // User has submitted a creation/change. Is it legal?
          val propsWithRawvals = rawProps.map { pair =>
            val (propIdStr, rawValue) = pair
            val propId = OID(propIdStr)
            val prop = state.prop(propId)
            (prop, rawValue)
          }
          val illegalVals = propsWithRawvals.filterNot(pair => pair._1.validate(pair._2))
          if (illegalVals.isEmpty) {
            // Everything parses, anyway, so send it on to the Space for processing:
            val propPairs = propsWithRawvals.map { pair =>
              val (prop, rawValue) = pair
              val value = prop.fromUser(rawValue)
              (prop.id, value)
            }
            val props = Thing.toProps(propPairs:_*)()
            val spaceMsg = if (thingId.isDefined) {
              // Editing an existing Thing
              ModifyThing(OID(spaceId), user.id, thingId.get, OID(info.model), props)
            } else {
              // Creating a new Thing
              CreateThing(OID(spaceId), user.id, OID(info.model), props)
            }
            askSpaceMgr[ThingResponse](spaceMsg) {
              case ThingFound(thingId, state) => Redirect(routes.Application.thing(state.id.toString, thingId.toString))
              case ThingFailed(msg) => {
                showEditPage(user, thing, oldModel, makeProps(rawProps), Some(msg))
              }
            }
          } else {
            // One or more values didn't parse against their PTypes. Give an error and continue:
            val badProps = illegalVals.map { pair => pair._1.displayName + " (" + pair._2 + ") " }
            val errorMsg = "Illegal values for " + badProps.mkString
            showEditPage(user, thing, oldModel, makeProps(rawProps), Some(errorMsg))
          }
        }
      }      
    )
  }
  
  def editThing(spaceId:String, thingIdStr:String) = withSpace(spaceId) { user => implicit state =>
    val thingId = OID(thingIdStr)
    val thing = state.anything(thingId)
    // TODO: security check that I'm allowed to edit this
    // TODO: error if the thing isn't found
    val model = state.anything(thing.model)
    showEditPage(user, Some(thing), model, PropList.from(thing))
  }
  
  def doEditThing(spaceId:String, thingIdStr:String) = editThingInternal(spaceId, Some(thingIdStr))

  def addCSS(spaceId:String, thingIdStr:String) = withSpace(spaceId) { user => implicit state =>
    val thingId = OID(thingIdStr)
    val thing = state.anything(thingId)
    // TODO: error if the thing isn't found
    Ok(views.html.addCSS(user, thing))
  }
  
  // TODO: limit the size of the uploaded file!!!
  def doAddCSS(spaceId:String, thingIdStr:String) = withSpaceAndRequest(spaceId) { user => implicit state => implicit request =>
    	val thingId = OID(thingIdStr)
        val thing = state.anything(thingId)
        
        request.body.asMultipartFormData flatMap(_.file("cssFile")) map { filePart =>
          val filename = filePart.filename
    	  val tempfile = filePart.ref.file
    	  // TODO: check whether the CSS contains any Javascript-enabling keywords
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
    	  val attachProps = Thing.toProps(DisplayNameProp(filename))()
    	  askSpaceMgr[ThingResponse](
    	    CreateAttachment(state.id, user.id, contents, AttachmentKind.CSS, contents.size, OIDs.RootOID, attachProps)) {
    	    case ThingFound(attachmentId, state2) => {
    	      val newProps = thing.props + (StylesheetProp(attachmentId))
    	      askSpaceMgr[ThingResponse](ModifyThing(state.id, user.id, thingId, thing.model, newProps)) {
    	        case ThingFound(thingIdAgain, state3) => {
    	          Redirect(routes.Application.thing(state.id.toString, thingId.toString))
    	        }
    	        case ThingFailed(msg) => {
                  Ok(views.html.addCSS(user, thing, Some(msg)))    	          
    	        }
    	      }
    	    }
    	    case ThingFailed(msg) => {
              Ok(views.html.addCSS(user, thing, Some(msg)))
    	    }
    	  }
    	} getOrElse {
          // TODO: return an error
          Ok(views.html.addCSS(user, thing))    	  
    	}
  }
  
  def attachment(spaceId:String, thingIdStr:String) = withUser { user => implicit request =>
    askSpaceMgr[AttachmentResponse](GetAttachment(OID(spaceId), user.id, OID(thingIdStr))) {
      case AttachmentContents(id, size, kind, content) => {
        kind match {
          case AttachmentKind.CSS => {
            Ok(content).as("text/css")
          }
          case _ => BadRequest
        }
      }
      case AttachmentFailed() => BadRequest
    }     
  }
  
  def userByName(userName:String) = {
    // TBD: Note that, for now at least, this simply returns my own spaces. I'm leery about
    // allowing people to see each other's spaces too casually.
    // TBD: is this the appropriate response to, say, "http://querki.net/jducoeur/"? Or
    // should this show the profile page instead?
    withUser { user => implicit request => 
      askSpaceMgr[ListMySpacesResponse](ListMySpaces(user.id)) { 
        case MySpaces(list) => Ok(views.html.spaces(Some(user), list))
      }
    }    
  }
  
  def spaceByName(userName:String, spaceName:String) = byName(userName, spaceName, None)
  def thingByName(userName:String, spaceName:String, thingName:String) = byName(userName, spaceName, Some(thingName))
  // TODO: this is currently a gaping security hole. It needs to become much more sophisticated. Add a
  // "withUserOptional", which passes through the credentials if found, but permits the operation
  // to play through regardless. It should check whether this user (or anonymous) is permitted to
  // see this thing once it is fetched.
  def byName[A](userName:String, spaceName:String, thingName:Option[String]) = Action {
    val userOpt = getUser(userName)
    if (userOpt.isDefined) {
      val owner = userOpt.get
        // TODO: pass the actual logged-in user (if any) to this request:
        askSpaceMgr[ThingResponse](GetThingByName(UnknownOID, owner.id, spaceName, thingName)) {
          case ThingFound(thingId, state) => {
            if (thingName.isDefined) {
              // TODO: this should show the logged-in user:
              Ok(views.html.thing(None, state.anything(thingId))(state))
            } else {
              // TODO: this should show the logged-in user:
              Ok(views.html.thing(None, state)(state))
            }
          }
          case ThingFailed(msg) => {
            // TODO: what's the right response here?
            Redirect(routes.Application.index)
          }
        }
    } else {
      // TODO: flash an error
      Redirect(routes.Application.index)
    }
  }
      
  def login = 
    Security.Authenticated(username, request => Ok(views.html.login(userForm))) { user =>
      Action { Redirect(routes.Application.index) }
    }
  
  def dologin = Action { implicit request =>
    userForm.bindFromRequest.fold(
      errors => BadRequest(views.html.index(None, errors, Some("I didn't understand that"))),
      user => {
        val lookedUp = User.get(user.name)
        if (lookedUp.isEmpty)
          BadRequest(views.html.login(userForm, Some("I don't know who you are")))
        else
	      Redirect(routes.Application.index).withSession(Security.username -> user.name)
      }
    )
  }
  
  def logout = Action {
    Redirect(routes.Application.index).withNewSession
  }
}