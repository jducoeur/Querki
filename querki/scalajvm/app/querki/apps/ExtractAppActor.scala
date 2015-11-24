package querki.apps

import akka.actor._

import org.querki.requester._

import models._

import querki.api.ProgressActor
import querki.data.TID
import querki.globals._
import querki.identity.User
import querki.time.DateTime
import querki.types.ModelTypeBase
import querki.util.QuerkiActor
import querki.values.SpaceState

/**
 * The Actor that shepherds the process of extracting a new App from an existing Space. This is separate
 * from the Space's main troupe, so that it can reboot the Space when needed.
 * 
 * Note that this is a ProgressActor, and follows that pattern of operation.
 * 
 * @author jducoeur
 */
private [apps] class ExtractAppActor(val ecology:Ecology, elements:Seq[TID], name:String, owner:User, state:SpaceState) 
  extends Actor with Requester with EcologyMember with ProgressActor 
{
  lazy val Core = interface[querki.core.Core]
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  lazy val System = interface[querki.system.System]
  
  lazy val SystemSpace = System.State
  lazy val systemId = SystemSpace.id
  
  implicit val s = state
  implicit val ec = ecology
  
  // The Things that need to be Extracted. Note that typeModels is a subset of models.
  case class Extractees(state:SpaceState, typeModels:Set[OID])
  var _extractees:Option[Extractees] = None
  
  override def receive = handleRequestResponse orElse handleProgress

  def calcProgress():Int = {
    // TODO
    20
  }
  
  def doWork():Unit = {
    // TEMP:
    QLog.spew("In ExtractAppActor:")
    elements foreach { elementId =>
      state.anything(ThingId(elementId.underlying)) match {
        case Some(element) => QLog.spewThing(element)(state)
        case _ => QLog.error(s"Unknown TID $elementId!")
      }
    }
    
    phaseDescription = "Backing up the Space"
    backupSpace()
    phaseDescription = "Figuring out everything to extract"
    _extractees = Some(computeExtractees())
  }
  
  import querki.db._
  /**
   * Before we start, do a backup to be on the safe side.
   * 
   * TODO: in principle, this should be extracted out to another Actor. In practice, it can
   * go away once we move to Akka Persistence anyway, so we're not going to worry about it too much.
   */
  def backupSpace() = {
    QDB(ShardKind.User) { implicit conn =>
      SpacePersistence.SpaceSQL(state.id, """
          CREATE TABLE {bname} LIKE {tname}
          """).executeUpdate
      SpacePersistence.SpaceSQL(state.id, """
          INSERT {bname} SELECT * FROM {tname}
          """).executeUpdate      
    }
  }
  
  /**
   * Creates the complete Extractees structure, with all the stuff we expect to pull out.
   * 
   * Note that this explicitly assumes there are no loops involved in the Model Types. That's a
   * fair assumption -- a lot of things will break if there are -- but do we need to sanity-check
   * for that?
   */
  def computeExtractees():Extractees = {
    val oids = elements
      .map(tid => ThingId(tid.underlying))
      .collect { case AsOID(oid) => oid }
    
    val init = Extractees(SpaceState(
      OID(1, 1),
      systemId,
      Map(Core.NameProp(name)),
      owner.mainIdentity.id,
      name,
      DateTime.now,
      Seq.empty,
      Some(SystemSpace),
      Map.empty,
      Map.empty,
      Map.empty,
      Map.empty,
      None
    ), Set.empty)
    
    (init /: oids) { (ext, elemId) => addThingToExtract(elemId, ext) }
  }
  
  def addThingToExtract(id:OID, in:Extractees):Extractees = {
    if (in.state.things.contains(id))
      // Already done
      in
    else {
      state.thing(AsOID(id)) match {
        case Some(t) => {
          if (t.spaceId == state.id) {
            // Add all the props to the list...
            val withProps = (in /: t.props.keys) { (ext, propId) => addPropToExtract(propId, ext) }
            // ... and this thing:
            in.copy(state = in.state.copy(things = in.state.things + (id -> t)))
          } else
            // Not local, so don't extract it
            in
        }
        // Hmm. This is conceptually an error, but it *can* happen, so we just have to give up here:
        case None => in
      }
    }
  }
  
  def addPropToExtract(id:OID, in:Extractees):Extractees = {
    if (in.state.spaceProps.contains(id))
      in
    else {
      state.prop(AsOID(id)) match {
        case Some(p) => {
          if (p.spaceId == state.id) {
            // Add meta-props, if any...
            val withProps = (in /: p.props.keys) { (ext, propId) => addPropToExtract(propId, ext) }
            // ... the Type...
            val withType = addTypeToExtract(p.pType, withProps)
            // ... and this Prop itself:
            withType.copy(state = in.state.copy(spaceProps = withType.state.spaceProps + (id -> p)))
          } else
            in
        }
        case None => in
      }
    }
  }
  
  def addTypeToExtract(pt:PType[_], in:Extractees):Extractees = {
    if (in.state.types.contains(pt.id))
      in
    else {
      if (pt.spaceId == state.id) {
        // If this is a model type, we need to dive in and add that:
        pt match {
          case mt:ModelTypeBase => {
            // We specifically note that this is a Model Type, because those do *not* get shadow copies
            // in the new Space:
            val withMT = in.copy(typeModels = in.typeModels + mt.basedOn)
            addThingToExtract(mt.basedOn, withMT)
          }
        }
      } else
        in
    }
  }
}

object ExtractAppActor {
  def props(e:Ecology, elements:Seq[TID], name:String, owner:User, state:SpaceState) = Props(classOf[ExtractAppActor], e, elements, name, owner, state) 
}
