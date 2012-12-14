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
  
  case class NewThingForm(fields:List[String], addedProperty:String, newModel:String, model:String)
  val newThingForm = Form(
    mapping(
      "field" -> list(text),
      "addedProperty" -> text,
      "newModel" -> text,
      "model" -> text
    )((field, addedProperty, newModel, model) => NewThingForm(field, addedProperty, newModel, model))
     ((info:NewThingForm) => Some((info.fields, info.addedProperty, info.newModel, info.model)))
  )
  
  def getUser(username:String):Option[User] = User.get(username)
  
  def username(request: RequestHeader) = request.session.get(Security.username)
  def forceUsername(request: RequestHeader) = username(request) orElse Some("")

  def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Application.login)

  def withAuth(requireLogin:Boolean)(f: => String => Request[AnyContent] => Result):Action[(Action[AnyContent], AnyContent)] = {
    val handler = { user:String =>
      Action(request => f(user)(request))
    }
    if (requireLogin)
      Security.Authenticated(username, onUnauthorized)(handler)
    else {
      // Note that forceUsername can never fail, it just returns empty string
      Security.Authenticated(forceUsername, onUnauthorized)(handler)
    }
  }
  
  def withUser(requireLogin:Boolean)(f: RequestContext => Result) = withAuth(requireLogin) { username => implicit request =>
    getUser(username).map { user =>
      f(RequestContext(request, Some(user), None, None))
    }.getOrElse {
      if (requireLogin)
        onUnauthorized(request)
      else
        // There isn't a legitimate logged-in user, but that's allowed for this call:
        f(RequestContext(request, None, None, None))
    }
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
   * Given a Space, this fetches that Space's current state before calling the core logic.
   * 
   * TBD: Why the ridiculous return signature? Because I am getting cryptic errors about withSpace
   * being recursive otherwise.
   */
  def withSpace(
        requireLogin:Boolean, 
        spaceId:String, 
        thingIdStr:Option[String] = None
      )(f: (RequestContext => Result)):Action[(Action[AnyContent], AnyContent)] = withUser(requireLogin) { rc =>
    val requesterId = rc.requester map (_.id) getOrElse UnknownOID
    // TODO: this should cope with a user string:
    // TODO: I believe the GetSpace method can in principle go away now:
    val thingId = thingIdStr map (ThingId(_))
    askSpaceMgr[ThingResponse](GetThing(requesterId, UnknownOID, ThingId(spaceId), thingId)) {
      case ThingFound(id, state) => {
        val thingOpt = id match {
          case UnknownOID => None
          case oid:OID => Some(state.anything(oid))
        }
        if (thingIdStr.isDefined && thingOpt.isEmpty)
          // TODO: a more appropriate error here. There might not be a user, so don't go to a page that
          // requires one:
          BadRequest(views.html.index(rc.requester, Some("Not a valid path")))
        else
          f(rc.copy(state = Some(state), thing = thingOpt))
      }
      case ThingFailed(msg) => Ok(views.html.index(rc.requester, Some(msg)))
    }     
  }

  def withThing(requireLogin:Boolean, spaceId:String, thingIdStr:String) = { 
    withSpace(requireLogin, spaceId, Some(thingIdStr)) _
  }
  
  def index = withUser(false) { rc =>
      Ok(views.html.index(rc.requester))
  }    

  def spaces = withUser(true) { rc => 
    askSpaceMgr[ListMySpacesResponse](ListMySpaces(rc.requester.get.id)) { 
      case MySpaces(list) => Ok(views.html.spaces(rc.requester.get, list))
    }
  }
    
  def space(spaceId:String) = withSpace(false, spaceId) { implicit rc =>
    Ok(views.html.thing(rc, rc.state.get))
  }
  
  def things(spaceId:String) = withSpace(false, spaceId) { implicit rc =>
    implicit val state = rc.state.get
    Ok(views.html.things(rc.requester))
  }
  
  def thing(spaceId:String, thingId:String) = withThing(false, spaceId, thingId) { implicit rc =>
    val chromelessFlag = rc.request.queryString.contains("cl")
    Ok(views.html.thing(rc, rc.thing.get, chromelessFlag))
  }
  
  def newSpace = withUser(true) { rc =>
    Ok(views.html.newSpace(rc.requester.get))
  }
  
  def doNewSpace = withUser(true) { rc =>
    implicit val request = rc.request
    val requester = rc.requester.get
    newSpaceForm.bindFromRequest.fold(
      errors => BadRequest(views.html.newSpace(requester, Some("You have to specify a legal space name"))),
      name => {
        if (NameProp.validate(name)) {
          askSpaceMgr[ThingResponse](CreateSpace(requester.id, name)) {
            case ThingFound(_, state) => Redirect(routes.Application.space(state.id.toThingId))
            case ThingFailed(msg) =>  BadRequest(views.html.newSpace(requester, Some(msg)))
          }
        } else {
          BadRequest(views.html.newSpace(requester, Some("That's not a legal Space name")))
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
  
  def createThing(spaceId:String) = withSpace(true, spaceId) { implicit rc =>
    implicit val state = rc.state.get
    showEditPage(rc.requester.get, None, SimpleThing, PropList((NameProp -> None)))
  }
  
  def doCreateThing(spaceId:String) = editThingInternal(spaceId:String, None)
  
  def editThingInternal(spaceId:String, thingIdStr:Option[String]) = withSpace(true, spaceId) { implicit rc =>
    implicit val request = rc.request
    implicit val state = rc.state.get
    val user = rc.requester.get
    val rawForm = newThingForm.bindFromRequest
    rawForm.fold(
      // TODO: correct error message here:
      errors => BadRequest(views.html.newSpace(rc.requester.get, Some("You have to specify a legal name"))),
      info => {
        // Whether we're creating or editing depends on whether thingIdStr is specified:
        val thingId = thingIdStr map (ThingId(_))
        val thing = thingId flatMap (state.anything(_))
        val rawProps = info.fields map { fieldId => (fieldId, rawForm("v-" + fieldId).value) }
        val oldModel = state.anything(OID(info.model))
        
        def makeProps(propList:List[(String, Option[String])]):PropList = {
          val rawList = propList.map { pair =>
            val (propIdStr, rawValue) = pair
            val propId = OID(propIdStr)
            val prop = state.prop(propId)
            (prop -> rawValue)
          }
          PropList(rawList:_*)
        }
        
        if (info.addedProperty.length > 0) {
          // User chose to add a Property; add that to the UI and continue:
          val allProps = rawProps :+ (info.addedProperty, Some(""))
          showEditPage(user, thing, oldModel, makeProps(allProps))
        } else if (info.newModel.length > 0) {
          // User is changing models. Replace the empty Properties with ones
          // appropriate to the new model, and continue:
          val model = state.anything(OID(info.newModel))
          showEditPage(user, thing, model, replaceModelProps(makeProps(rawProps), model))
        } else {
          // User has submitted a creation/change. Is it legal?
          val propsWithRawvals = rawProps.map { pair =>
            val (propIdStr, rawValueOpt) = pair
            val propId = OID(propIdStr)
            val prop = state.prop(propId)
            // This is mainly for checkboxes -- those don't return *anything* if they are not
            // checked:
            val rawValue = if (rawValueOpt.isDefined) rawValueOpt.get else prop.renderedDefault.raw.toString
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
              ModifyThing(ThingId(spaceId), user.id, thingId.get, OID(info.model), props)
            } else {
              // Creating a new Thing
              CreateThing(ThingId(spaceId), user.id, OID(info.model), props)
            }
            askSpaceMgr[ThingResponse](spaceMsg) {
              case ThingFound(thingId, state) => Redirect(routes.Application.thing(state.id.toThingId, thingId.toThingId))
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
  
  def editThing(spaceId:String, thingIdStr:String) = withSpace(true, spaceId) { implicit rc =>
    implicit val state = rc.state.get
    val thingId = ThingId(thingIdStr)
    val thingOpt = state.anything(thingId)
    thingOpt map { thing =>
	  // TODO: security check that I'm allowed to edit this
	  val model = state.anything(thing.model)
	  showEditPage(rc.requester.get, Some(thing), model, PropList.from(thing))
    } getOrElse {
      // TODO: flash an error
      Redirect(routes.Application.thing(spaceId, thingIdStr))
    }
  }
  
  def doEditThing(spaceId:String, thingIdStr:String) = editThingInternal(spaceId, Some(thingIdStr))

  def upload(spaceId:String) = withSpace(true, spaceId) { implicit rc =>
    implicit val state = rc.state.get
    Ok(views.html.upload(rc.requester.get))
  }
  
  // TODO: I think this function is the straw that breaks the camel's back when it comes to code structure.
  // It has too many nested levels, and they're only going to get worse. It needs a big rewrite.
  //
  // A better solution is probably to use 2.10's Try monad pretty rigorously. Say that askSpaceMgr returns
  // a monadic response, that composes by calling the next step in the chain. If you get ThingFound, we
  // continue; if you get ThingFailed, we fail the Try with an error. Done right, and all of this can
  // become a nice for() statement, and will be much easier to reason about in the long run. That will
  // have to wait until we have 2.10 going, though.
  //
  // TODO: generalize upload. You should be able to select the desired file type from a whitelist,
  // and upload that type properly. There should be type-specific validation -- for instance, CSS should
  // be Javascript-scrubbed. There should be type-specific post-processing -- for instance, photos
  // should automatically generate thumbnails.
  //
  // TODO: limit the size of the uploaded file!!!
  def doUpload(spaceId:String) = withSpace(true, spaceId) { implicit rc =>
    implicit val state = rc.state.get
    val user = rc.requester.get
    rc.request.body.asMultipartFormData flatMap(_.file("uploadedFile")) map { filePart =>
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
	  val attachProps = Thing.toProps(DisplayNameProp(filename))()
	  askSpaceMgr[ThingResponse](
	    CreateAttachment(state.id, user.id, contents, MIMEType.JPEG, contents.size, OIDs.PhotoBaseOID, attachProps)) {
	    case ThingFound(attachmentId, state2) => {
	      Redirect(routes.Application.thing(state.id.toString, attachmentId.toString))
	    }
	    case ThingFailed(msg) => {
          Ok(views.html.upload(user, Some(msg)))
	    }
	  }
	} getOrElse {
      Ok(views.html.upload(user, Some("You didn't specify a file")))
	}
  }

  // TODO: this should go away, in favor of the more robust attachmentByName
  def attachment(spaceId:String, thingIdStr:String) = Action {
    askSpaceMgr[AttachmentResponse](GetAttachment(UnknownOID, UnknownOID, ThingId(spaceId), ThingId(thingIdStr))) {
      case AttachmentContents(id, size, mime, content) => {
        Ok(content).as(mime)
      }
      case AttachmentFailed() => BadRequest
    }     
  }
  // TODO: deal with security here. This is another place where we need to take an *optional* user ID,
  // and let the Space decide whether to allow access.
  // TODO: should/can this be refactored with byName below?
  def attachmentByName(ownerName:String, spaceName:String, thingName:String) = Action {
    val userOpt = getUser(ownerName)
    if (userOpt.isDefined) {
      val owner = userOpt.get
      Logger.info("About to request the attachment!")
      askSpaceMgr[AttachmentResponse](GetAttachment(UnknownOID, owner.id, AsName(spaceName), AsName(thingName))) {
        case AttachmentContents(id, size, mime, content) => {
          Ok(content).as(mime)
        }
        case AttachmentFailed() => BadRequest
      }
    } else {
      // TODO: flash an error
      Redirect(routes.Application.index)      
    }
  }
  
  def userByName(userName:String) = {
    // TBD: Note that, for now at least, this simply returns my own spaces. I'm leery about
    // allowing people to see each other's spaces too casually.
    // TBD: is this the appropriate response to, say, "http://querki.net/jducoeur/"? Or
    // should this show the profile page instead?
    withUser(true) { rc => 
      askSpaceMgr[ListMySpacesResponse](ListMySpaces(rc.requester.get.id)) { 
        case MySpaces(list) => Ok(views.html.spaces(rc.requester.get, list))
      }
    }    
  }
  
  def spaceByName(userName:String, spaceName:String) = byName(userName, spaceName, None)
  def thingByName(userName:String, spaceName:String, thingName:String) = byName(userName, spaceName, Some(thingName))
  // TODO: this is currently a gaping security hole. It needs to become much more sophisticated. Add a
  // "withUserOptional", which passes through the credentials if found, but permits the operation
  // to play through regardless. It should check whether this user (or anonymous) is permitted to
  // see this thing once it is fetched.
  // TODO: merge this with thing() -- this is just a fancier version. Indeed, there's a big refactor
  // waiting in here -- all this byName stuff should just become more regular.
  def byName[A](userName:String, spaceName:String, thingName:Option[String]) = Action { implicit request =>
    val userOpt = getUser(userName)
    if (userOpt.isDefined) {
      val owner = userOpt.get
      val chromelessFlag = request.queryString.contains("cl")
        // TODO: pass the actual logged-in user (if any) to this request:
        askSpaceMgr[ThingResponse](GetThing(UnknownOID, owner.id, ThingId(spaceName), thingName map (ThingId(_)))) {
          case ThingFound(thingId, state) => {
            val thing = state.anything(thingId)
            // TODO: this should show the logged-in user:
            val rc = RequestContext(request, None, Some(state), Some(thing))
            if (thingName.isDefined) {
              Ok(views.html.thing(rc, thing, chromelessFlag))
            } else {
              // TODO: this should show the logged-in user:
              Ok(views.html.thing(rc, state, chromelessFlag))
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
      errors => BadRequest(views.html.login(errors, Some("I didn't understand that"))),
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