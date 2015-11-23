package querki.apps

import akka.actor._

import org.querki.requester._

import models._

import querki.api.ProgressActor
import querki.data.TID
import querki.globals._
import querki.util.QuerkiActor

/**
 * The Actor that shepherds the process of extracting a new App from an existing Space. This is separate
 * from the Space's main troupe, so that it can reboot the Space when needed.
 * 
 * Note that this is a ProgressActor, and follows that pattern of operation.
 * 
 * @author jducoeur
 */
private [apps] class ExtractAppActor(val ecology:Ecology, elements:Seq[TID], state:SpaceState) 
  extends Actor with Requester with EcologyMember with ProgressActor 
{
  lazy val SpacePersistence = interface[querki.spaces.SpacePersistence]
  
  implicit val s = state
  implicit val ec = ecology
  
  // The Things that need to be Extracted. Note that typeModels is a subset of models.
  case class Extractees(instances:Set[OID], models:Set[OID], props:Set[OID], types:Set[OID], typeModels:Set[OID])
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
   * Note that this explicitly assumes there are no loops involved in the Model Types.
   * TODO: is this a safe assumption? Maybe not. Investigate...
   */
  def computeExtractees():Extractees = {
    ???
  }
  
  def addThingToExtract(id:OID, in:Extractees):Extractees = {
    if (in.instances.contains(id) || in.models.contains(id))
      // Already done
      in
    else {
      state.thing(AsOID(id)) match {
        case Some(t) => {
          if (t.spaceId == state.id) {
            // Add all the props to the list...
            val withProps = (in /: t.props.keys) { (ext, propId) => addPropToExtract(propId, ext) }
            // ... and this thing:
            if (t.isModel)
              in.copy(models = in.models + id)
            else
              in.copy(instances = in.instances + id)
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
    if (in.props.contains(id))
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
            withType.copy(props = withType.props + id)
          } else
            in
        }
        case None => in
      }
    }
  }
  
  def addTypeToExtract(id:PType[_], in:Extractees):Extractees = {
    ???
  }
}

object ExtractAppActor {
  def props(e:Ecology, elements:Seq[TID], state:SpaceState) = Props(classOf[ExtractAppActor], e, elements, state) 
}
