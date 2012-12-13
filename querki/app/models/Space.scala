package models

import akka.actor._
import akka.pattern.ask
import akka.util.duration._
import akka.util.Timeout

import play.api._
import play.api.Configuration
import play.api.Play
import play.api.Play.current
import play.api.libs.concurrent._
import play.Configuration

// Database imports:
import anorm._
import play.api.db._
import play.api.Play.current

import Kind._
import Thing._

import system._
import system.OIDs._
import system.SystemSpace._

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
    spaceProps:Map[OID, Property[_,_,_]],
    things:Map[OID, ThingState],
    colls:Map[OID, Collection[_]]) 
  extends Thing(s, s, m, Kind.Space, pf) 
{
  // Walks up the App tree, looking for the specified Thing of the implied type:
  def resolve[T <: Thing](tid:OID)(lookup: (SpaceState, OID) => Option[T]):T = {
    lookup(this, tid).getOrElse(
          app.map(_.resolve(tid)(lookup)).getOrElse(throw new Exception("Couldn't find " + tid)))
  }
  def typ(ptr:OID) = resolve(ptr) (_.types.get(_))
  def prop(ptr:OID) = resolve(ptr) (_.spaceProps.get(_))
  def thing(ptr:OID) = resolve(ptr) (_.things.get(_))
  def coll(ptr:OID) = resolve(ptr) (_.colls.get(_))
  
  def anything(oid:OID):Thing = {
    // TODO: this should do something more sensible if the OID isn't found at all:
    things.getOrElse(oid, 
        spaceProps.getOrElse(oid, 
            types.getOrElse(oid, 
                colls.getOrElse(oid,
                	app.map(_.anything(oid)).getOrElse(this)))))
  }
  
  def thingWithName(name:String, things:Map[OID, Thing]):Option[Thing] = {
    things.values.find { thing =>
      val thingNameOpt = thing.canonicalName
      thingNameOpt.isDefined && NameType.equalNames(thingNameOpt.get, name)
    }
  }
  
  // TBD: should this try recognizing Display Names as well? I've confused myself that way once
  // or twice.
  def anythingByName(rawName:String):Option[Thing] = {
    val name = NameType.toInternal(rawName)
    thingWithName(name, things).orElse(
      thingWithName(name, spaceProps).orElse(
        thingWithName(name, types).orElse(
          thingWithName(name, colls))))
  }
  
  def allProps:Map[OID, Property[_,_,_]] = if (app.isEmpty) spaceProps else spaceProps ++ app.get.allProps
  
  def allModels:Iterable[ThingState] = {
    implicit val s = this
    val myModels = things.values.filter(_.first(IsModelProp))
    if (app.isEmpty) {
      myModels
    } else {
      myModels ++ app.get.allModels
    }
  }
}


import MIMEType.MIMEType

/**
 * ThingId deals with the fact that we sometimes are passing OIDs around in messages, and
 * sometimes OIDs. This provides an abstraction that lets you use either in a sensible way.
 */
sealed trait ThingId
case class AsOID(oid:OID) extends ThingId
case class AsName(name:String) extends ThingId
object UnknownThingId extends AsOID(UnknownOID)
object ThingId {
  def apply(str:String):ThingId = {
    str(0) match {
      case '.' => AsOID(OID(str.substring(1)))
      case _ => AsName(str)
    }
  }
}

/**
 * The base class for message that get routed to a Space. Note that owner is only relevant if
 * the spaceId is AsName (which needs to be disambiguated by owner).
 */
sealed class SpaceMessage(val requester:OID, val owner:OID, val spaceId:ThingId) extends SpaceMgrMsg
object SpaceMessage {
  def unapply(input:SpaceMessage) = Some((input.requester, input.owner, input.spaceId))
}

case class CreateThing(space:OID, req:OID, modelId:OID, props:PropMap) extends SpaceMessage(req, UnknownOID, AsOID(space))

case class CreateAttachment(space:OID, req:OID, 
    content:Array[Byte], mime:MIMEType, size:Int, 
    modelId:OID, props:PropMap) extends SpaceMessage(req, UnknownOID, AsOID(space))

case class GetAttachment(req:OID, own:OID, space:ThingId, attachId:ThingId) extends SpaceMessage(req, own, space)

case class ModifyThing(space:OID, req:OID, id:OID, modelId:OID, props:PropMap) extends SpaceMessage(req, UnknownOID, AsOID(space))

case class CreateProperty(id:OID, req:OID, model:OID, pType:OID, cType:OID, props:PropMap) extends SpaceMessage(req, UnknownOID, AsOID(id))

case class GetThing(req:OID, own:OID, space:ThingId, thing:Option[ThingId]) extends SpaceMessage(req, own, space)

// This is the most common response when you create/fetch any sort of Thing
sealed trait ThingResponse
case class ThingFound(id:OID, state:SpaceState) extends ThingResponse
case class ThingFailed(msg:String) extends ThingResponse

sealed trait AttachmentResponse
case class AttachmentContents(id:OID, size:Int, mime:MIMEType, content:Array[Byte]) extends AttachmentResponse
case class AttachmentFailed() extends AttachmentResponse



/**
 * The Actor that encapsulates a Space.
 * 
 * As a hard and fast rule, application code in Querki never alters a Space directly.
 * Instead, all changes are described as messages to the Space's Actor; that is
 * responsible for making the actual changes. This way, the database layer is firmly
 * encapsulated, and race conditions are prevented.
 * 
 * Similarly, you fetch the Space's current State from the Space Actor. The State is
 * an immutable object completely describing the Space at a single moment; you are
 * allowed to process that in any way, just not change it.
 * 
 * An Actor's name is based on its OID, as is its Thing Table in the DB. Use Space.sid()
 * to get the name. Note that this has *nothing* to do with the Space's Display Name, which
 * is user-defined. (And unique only to that user.)
 */
class Space extends Actor {
  
  import context._
  import models.system.SystemSpace.{State => systemState, _} 
  
  def id = OID(self.path.name)
  var _currentState:Option[SpaceState] = None
  // This should return okay any time after preStart:
  def state = {
    _currentState match {
      case Some(s) => s
      case None => throw new Exception("State not ready in Actor " + id)
    }
  }
  // TODO: keep the previous states here in the Space, so we can undo/history
  def updateState(newState:SpaceState) = {
    _currentState = Some(newState)
  }
  
  def SpaceSQL(query:String) = Space.SpaceSQL(id, query)
  def AttachSQL(query:String) = Space.AttachSQL(id, query)
  
  /**
   * Fetch the high-level information about this Space. Note that this will throw
   * an exception if for some reason we can't load the record. 
   */
  lazy val spaceInfo = {
    DB.withTransaction { implicit conn =>
      SQL("""
          select * from Spaces where id = {id}
          """).on("id" -> id.raw).apply().headOption.get
    }    
  }
  lazy val name = spaceInfo.get[String]("name").get
  lazy val owner = OID(spaceInfo.get[Long]("owner").get)
  
  // TODO: this needs to become real. For now, everything is world-readable.
  def canRead(who:OID):Boolean = {
    true
  }
  
  // TODO: this needs to become much more sophisticated. But for now, it's good enough
  // to say that only the owner can edit:
  def canCreateThings(who:OID):Boolean = {
    who == owner
  }
  
  // TODO: this needs to become much more sophisticated. But for now, it's good enough
  // to say that only the owner can edit:
  def canEdit(who:OID, thingId:OID):Boolean = {
    who == owner
  }
  
  def loadSpace() = {
    // TODO: we need to walk up the tree and load any ancestor Apps before we prep this Space
    DB.withTransaction { implicit conn =>
      // The stream of all of the Things in this Space:
      val stateStream = SpaceSQL("""
          select * from {tname}
          """)()
      // Split the stream, dividing it by Kind:
      val streamsByKind = stateStream.groupBy(_.get[Int]("kind").get)
      
      // Now load each kind. We do this in order, although in practice it shouldn't
      // matter too much so long as Space comes last:
      def getThingStream[T <: Thing](kind:Int)(builder:(OID, OID, PropMap) => T):Stream[T] = {
        streamsByKind.get(kind).getOrElse(Stream.Empty).map { row =>
          // TODO: the app shouldn't be hardcoded to SystemSpace
          val propMap = Thing.deserializeProps(row.get[String]("props").get, systemState)
          builder(OID(row.get[Long]("id").get), OID(row.get[Long]("model").get), propMap)
        }
      }
      def getThings[T <: Thing](kind:Int)(builder:(OID, OID, PropMap) => T):Map[OID, T] = {
        val tStream = getThingStream(kind)(builder)
        (Map.empty[OID, T] /: tStream)((m, t) => m + (t.id -> t))
      }
      
      val props = getThings(Kind.Property) { (thingId, modelId, propMap) =>
        val typ = systemState.typ(TypeProp.first(propMap))
        // This cast is slightly weird, but safe and should be necessary. But I'm not sure
        // that the PTypeBuilder part is correct -- we may need to get the RT correct.
        val boundTyp = typ.asInstanceOf[PType[typ.valType] with PTypeBuilder[typ.valType, Any]]
        val coll = systemState.coll(CollectionProp.first(propMap))
        val boundColl = coll.asInstanceOf[Collection[coll.implType]]
        new Property(thingId, id, modelId, boundTyp, boundColl, () => propMap)
      }
      
      val things = getThings(Kind.Thing) { (thingId, modelId, propMap) =>
        new ThingState(thingId, id, modelId, () => propMap)        
      }
      
      val attachments = getThings(Kind.Attachment) { (thingId, modelId, propMap) =>
        new ThingState(thingId, id, modelId, () => propMap, Kind.Attachment)        
      }
      
      val allThings = things ++ attachments
      
      val spaceStream = getThingStream(Kind.Space) { (thingId, modelId, propMap) =>
        new SpaceState(
             thingId,
             modelId,
             () => propMap,
             owner,
             name,
             Some(systemState),
             // TODO: dynamic PTypes
             Map.empty[OID, PType[_]],
             props,
             allThings,
             // TODO (probably rather later): dynamic Collections
             Map.empty[OID, Collection[_]]
            )
      }
      // TBD: note that it is a hard error if there aren't any Spaces found. We expect exactly one:
      _currentState = Some(spaceStream.head)
    }    
  }
  
  override def preStart() = {
    loadSpace()
  } 
  
  def createSomething(spaceId:OID, who:OID, modelId:OID, props:PropMap, kind:Kind)(
      otherWork:OID => java.sql.Connection => Unit) = 
  {
    val name = NameProp.firstOpt(props)
    if (!canCreateThings(who))
      sender ! ThingFailed("You are not allowed to create anything in this Space")
    else if (name.isDefined && state.anythingByName(name.get).isDefined)
      sender ! ThingFailed("This Space already has a Thing with that name")
    else DB.withTransaction { implicit conn =>
      val thingId = OID.next
      val thing = ThingState(thingId, spaceId, modelId, () => props, kind)
      // TODO: add a history record
      Space.createThingInSql(thingId, spaceId, modelId, kind, props, systemState)
      updateState(state.copy(things = state.things + (thingId -> thing)))
      // This callback intentionally takes place inside the transaction:
      otherWork(thingId)(conn)
        
      sender ! ThingFound(thingId, state)
    }    
  }
  
  def receive = {
    case req:CreateSpace => {
      sender ! RequestedSpace(state)
    }
    
    case GetSpace(spaceId, requesterId) => {
      if (!canRead(requesterId))
        sender ! ThingFailed
      else
        sender ! RequestedSpace(state)
    }
    
    case CreateThing(spaceId, who, modelId, props) => {
      createSomething(spaceId, who, modelId, props, Kind.Thing) { thingId => implicit conn => Unit }
    }
    
    case CreateAttachment(spaceId, who, content, mime, size, modelId, props) => {
      createSomething(spaceId, who, modelId, props, Kind.Attachment) { thingId => implicit conn =>
      	AttachSQL("""
          INSERT INTO {tname}
          (id, mime, size, content) VALUES
          ({thingId}, {mime}, {size}, {content})
        """
        ).on("thingId" -> thingId.raw,
             "mime" -> mime,
             "size" -> size,
             "content" -> content).executeUpdate()        
      }
    }
    
    case GetAttachment(who, owner, space, attachId) => {
      if (!canRead(who))
        sender ! AttachmentFailed
      else DB.withTransaction { implicit conn =>
        val attachOid = attachId match {
          case AsOID(oid) => oid
          // TODO: handle the case where this name is not recognized:
          case AsName(name) => {
            state.anythingByName(name).get.id
          }
        }
        // TODO: this will throw an error if the specified attachment doesn't exist
        // Guard against that.
        val results = AttachSQL("""
            SELECT mime, size, content FROM {tname} where id = {id}
            """).on("id" -> attachOid.raw)().map {
          // TODO: we really, really must not hardcode this type below. This is some sort of
          // serious Anorm driver failure, I think. As it stands, it's going to crash when we
          // move to MySQL:
          case Row(mime:MIMEType, size:Int, content:org.h2.jdbc.JdbcBlob) => {
            AttachmentContents(attachOid, size, mime, content.getBytes(1, content.length().toInt))
          }
        }.head
        sender ! results
      }
    }
    
    case ModifyThing(spaceId, who, thingId, modelId, newProps) => {
      Logger.info("In ModifyThing")
      if (!canEdit(who, thingId)) {
        Logger.info(who.toString + " can't edit -- requires " + owner.toString)
        sender ! ThingFailed
      } else DB.withTransaction { implicit conn =>
        Logger.info("About to get the thing")
        val oldThing = state.thing(thingId)
        Logger.info("Got the thing")
        // TODO: compare properties, build a history record of the changes
        val newThingState = oldThing.copy(m = modelId, pf = () => newProps)
        Logger.info("Created newThingState")
        Space.SpaceSQL(spaceId, """
          UPDATE {tname}
          SET model = {modelId}, props = {props}
          WHERE id = {thingId}
          """
          ).on("thingId" -> thingId.raw,
               "modelId" -> modelId.raw,
               "props" -> Thing.serializeProps(newProps, state)).executeUpdate()    
        updateState(state.copy(things = state.things + (thingId -> newThingState)))
        
        Logger.info("About to leave ModifyThing")
        sender ! ThingFound(thingId, state)
      }
    }
    
    case GetThing(req, owner, space, thingIdOpt) => {
      if (!canRead(req))
        sender ! ThingFailed("Space not found")
      else if (thingIdOpt.isDefined) {
        val thingOpt = thingIdOpt.get match {
          case AsOID(oid) => Some(state.anything(oid))
          case AsName(name) => state.anythingByName(name)
        }
        if (thingOpt.isDefined) {
          sender ! ThingFound(thingOpt.get.id, state)
        } else {
          // TODO: better error message!
          sender ! ThingFailed("Thing not found")
        }
      } else {
        // TODO: is this the most semantically appropriate response?
        sender ! ThingFound(UnknownOID, state)
      }
    }
  }
}

object Space {
  // The name of the Space Actor
  def sid(id:OID) = id.toString
  // The OID of the Space, based on the sid
  def oid(sid:String) = OID(sid)
  // The name of the Space's Thing Table
  def thingTable(id:OID) = "s" + sid(id)
  // The name of the Space's History Table
  def historyTable(id:OID) = "h" + sid(id)
  // The name of the Space's Attachments Table
  def attachTable(id:OID) = "a" + sid(id)

  /**
   * A Map of Things, suitable for passing into a SpaceState.
   */
  def oidMap[T <: Thing](items:T*):Map[OID,T] = {
    (Map.empty[OID,T] /: items) ((m, i) => m + (i.id -> i))
  }
  
  /**
   * The intent here is to use this with queries that use the thingTable. You can't use
   * on()-style parameters for table names, so we need to work around that.
   * 
   * You can always use this in place of ordinary SQL(); it is simply a no-op for ordinary queries.
   */
  def SpaceSQL(spaceId:OID, query:String):SqlQuery = SQL(query.replace("{tname}", thingTable(spaceId)))
  def createThingInSql(thingId:OID, spaceId:OID, modelId:OID, kind:Int, props:PropMap, serialContext:SpaceState)(implicit conn:java.sql.Connection) = {
    SpaceSQL(spaceId, """
        INSERT INTO {tname}
        (id, model, kind, props) VALUES
        ({thingId}, {modelId}, {kind}, {props})
        """
        ).on("thingId" -> thingId.raw,
             "modelId" -> modelId.raw,
             "kind" -> kind,
             "props" -> Thing.serializeProps(props, serialContext)).executeUpdate()    
  }
  
  def AttachSQL(spaceId:OID, query:String):SqlQuery = SQL(query.replace("{tname}", attachTable(spaceId)))
}

sealed trait SpaceMgrMsg

// TODO: I don't think this is used any more?
case class GetSpace(id:OID, requester:OID) extends SpaceMgrMsg
sealed trait GetSpaceResponse
case class RequestedSpace(state:SpaceState) extends GetSpaceResponse
case class GetSpaceFailed(id:OID, msg:String) extends GetSpaceResponse

case class ListMySpaces(owner:OID) extends SpaceMgrMsg
sealed trait ListMySpacesResponse
case class MySpaces(spaces:Seq[(OID,String)]) extends ListMySpacesResponse

// This responds eventually with a RequestedSpace:
case class CreateSpace(owner:OID, name:String) extends SpaceMgrMsg

class SpaceManager extends Actor {
  import models.system.SystemSpace
  import SystemSpace._
  import Space._
  
//  // The local cache of Space States.
//  // TODO: this needs to age properly.
//  // TODO: this needs a cap of how many states we will try to cache.
//  var spaceCache:Map[OID,SpaceState] = Map.empty
//  
//  def addState(state:SpaceState) = spaceCache += (state.id -> state)
//  
//  // The System Space is hardcoded, and we create it at the beginning of time:
//  addState(system.SystemSpace.State)
  
  // TEMP:
  val replyMsg = Play.configuration.getString("querki.test.replyMsg").getOrElse("MISSING REPLY MSG!")
  
  def getSpace(spaceId:OID):ActorRef = {
    val sid = Space.sid(spaceId)
 	// TODO: this *should* be using context.child(), but that doesn't exist in Akka
    // 2.0.2, so we have to wait until we have access to 2.1.0:
    //val childOpt = context.child(sid)
    val childOpt = context.children find (_.path.name == sid)
    childOpt match {
      case Some(child) => child
      case None => context.actorOf(Props[Space], sid)
    }
  }
  
  def receive = {
    case req:ListMySpaces => {
      //val results = spaceCache.values.filter(_.owner == req.owner).map(space => (space.id, space.name)).toSeq
      if (req.owner == SystemUserOID)
        sender ! MySpaces(Seq((systemOID, State.name)))
      else {
        // TODO: this involves DB access, so should be async using the Actor DSL
        val results = DB.withConnection { implicit conn =>
          val spaceStream = SQL("""
              select id, display from Spaces
              where owner = {owner}
              """).on("owner" -> req.owner.raw)()
          spaceStream.force.map { row =>
            val id = OID(row.get[Long]("id").get)
            val name = row.get[String]("display").get
            (id, name)
          }
        }
        sender ! MySpaces(results)
      }
    }
    
    // TODO: I don't think this is used any more?
    case req:GetSpace => {
      if (req.id == systemOID)
        sender ! RequestedSpace(State)
      else
        getSpace(req.id).forward(req)
//      val cached = spaceCache.get(req.id)
//      if (cached.nonEmpty)
//        sender ! RequestedSpace(cached.get)
//      else
//        sender ! GetSpaceFailed(req.id)
    }
    
    case req:CreateSpace => {
      // TODO: technically, the legal name check should happen in the same transactions as
      // space creation, to avoid the just-barely-possible race condition of creating the
      // same name in two different sessions simultaneously. Extraordinarily unlikely, but
      // we should fix this.
      val errorMsg = legalSpaceName(req.owner, req.name)
      if (errorMsg.isEmpty) {
        // TODO: check that the owner hasn't run out of spaces he can create
        // TODO: check that the owner doesn't already have a space with that name
        // TODO: this involves DB access, so should be async using the Actor DSL
        val (spaceId, spaceActor) = createSpace(req.owner, req.name)
        // Now, let the Space Actor finish the process once it is ready:
        spaceActor.forward(req)
      } else {
        sender ! GetSpaceFailed(UnknownOID, errorMsg.get)
      }
    }

    // TODO: CRITICAL: we need a pseudo-Space for System!
    // This clause is a pure forwarder for messages to a particular Space.
    // Is there a better way to do this?
    case req:SpaceMessage => {
      Logger.info("SpaceMgr got " + req)
      // TODO: cope with messages in name style instead
      req match {
        case SpaceMessage(_, _, AsOID(spaceId)) => getSpace(spaceId).forward(req)
        case SpaceMessage(_, ownerId, AsName(spaceName)) => {
          val spaceOpt = getSpaceByName(ownerId, spaceName)
          // TODO: the error clause below potentially leaks information about whether a
          // give space exists for an owner. Examine the various security paths holistically.
          spaceOpt map getSpace map { _.forward(req) } getOrElse { sender ! ThingFailed("Not a legal path") }
        }
      }
    }
  }
  
  private def getSpaceByName(ownerId:OID, name:String):Option[OID] = {
    DB.withTransaction { implicit conn =>
      val rowOption = SQL("""
          SELECT id from Spaces WHERE owner = {ownerId} AND name = {name}
          """).on("ownerId" -> ownerId.raw, "name" -> NameType.canonicalize(name))().headOption
      rowOption.map(row => OID(row.get[Long]("id").get))
    }
  }
  
  private def legalSpaceName(ownerId:OID, name:String):Option[String] = {
    def numWithName = DB.withTransaction { implicit conn =>
      SQL("""
          SELECT COUNT(*) AS c from Spaces WHERE owner = {ownerId} AND name = {name}
          """).on("ownerId" -> ownerId.raw, "name" -> NameType.canonicalize(name)).apply().headOption.get.get[Long]("c").get
    }
    if (!NameProp.validate(name))
      Some("That's not a legal name for a Space")
    else if (numWithName > 0) {
      Logger.info("numWithName = " + numWithName)
      Some("You already have a Space with that name")
    } else
      None
  }
  
  private def createSpace(owner:OID, display:String) = {
    val name = NameType.canonicalize(display)
    val spaceId = OID.next
    Logger.info("Creating new Space with OID " + Space.thingTable(spaceId))
    // Why the replace() here? It looks to me like the .on() function always
    // surrounds its parameter with quotes, and I don't think that works in
    // the table name. Sad.
    DB.withTransaction { implicit conn =>
      SpaceSQL(spaceId, """
          CREATE TABLE {tname} (
            id bigint NOT NULL,
            model bigint NOT NULL,
            kind int NOT NULL,
            props clob NOT NULL,
            PRIMARY KEY (id))
          """).executeUpdate()
      AttachSQL(spaceId, """
          CREATE TABLE {tname} (
            id bigint NOT NULL,
            mime varchar(127) NOT NULL,
            size int NOT NULL,
            content blob NOT NULL,
            PRIMARY KEY (id))
          """).executeUpdate()
      SQL("""
          INSERT INTO Spaces
          (id, shard, name, display, owner, size) VALUES
          ({sid}, {shard}, {name}, {display}, {ownerId}, 0)
          """).on("sid" -> spaceId.raw, "shard" -> 1.toString, "name" -> name,
                  "display" -> display, "ownerId" -> owner.raw).executeUpdate()
      val initProps = Thing.toProps(Thing.setName(name), DisplayNameProp(display))()
      Space.createThingInSql(spaceId, spaceId, RootOID, Kind.Space, initProps, State)
    }
    val spaceActor = context.actorOf(Props[Space], name = Space.sid(spaceId))
    (spaceId, spaceActor)
  }
}

object SpaceManager {
  // I don't love having to hold a static reference like this, but Play's statelessness
  // probably requires that. Should we instead be holding a Path, and looking it up
  // each time?
  lazy val ref = Akka.system.actorOf(Props[SpaceManager], name="SpaceManager")
  
  // This is probably over-broad -- we're going to need the timeout to push through to
  // the ultimate callers.
  implicit val timeout = Timeout(5 seconds)
  
  // Send a message to the SpaceManager, expecting a return of type A to be
  // passed into the callback. This wraps up the messy logic to go from a
  // non-actor-based Play environment to the SpaceManager. We'll likely
  // generalize it further eventually.
  //
  // Type A is the response we expect to get back from the message, which will
  // be sent to the given callback.
  //
  // Type B is the type of the callback. I'm a little surprised that this isn't
  // inferred -- I suspect I'm doing something wrong syntactically.
  def ask[A,B](msg:SpaceMgrMsg)(cb: A => B)(implicit m:Manifest[A]):Promise[B] = {
    (ref ? msg).mapTo[A].map(cb).asPromise
  }
}