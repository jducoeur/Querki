package querki.editing

import akka.actor._
import org.querki.requester._
import models._
import querki.api.ProgressActor
import querki.globals._
import querki.identity.User
import querki.spaces.messages._
import querki.values.RequestContext

/**
 * Removing a non-empty Property is a bit fraught, and requires touching a lot of Things.
 * So we do it with a worker Actor. Note that this mixes ProgressActor in, to keep the Client
 * apprised of what's up.
 * 
 * @author jducoeur
 */
class RemovePropertyActor(rc: RequestContext, propId:OID, val ecology:Ecology, state:SpaceState, router:ActorRef)
  extends Actor with Requester with ProgressActor with EcologyMember
{
  lazy val Core = interface[querki.core.Core]
  lazy val DataModelAccess = interface[querki.datamodel.DataModelAccess]
  lazy val Editor = interface[Editor]
  
  lazy val InstanceProps = Editor.InstanceProps
  lazy val LinkType = Core.LinkType

  implicit val s = state
  implicit val e = ecology
  
  lazy val prop = state.prop(propId).get
  lazy val propName = prop.displayName
  
  var instances = Iterable.empty[Thing]
  var models = Iterable.empty[Thing]
  def nTotal = instances.size + models.size
  var nDone = 0
  var finished = false
  
  def calcProgress():Int = {
    if (finished)
      100
    else
      ((nDone.toFloat / nTotal) * 90).toInt
  }
  
  def doWork():Unit = {
    phaseDescription = "Figuring out the process"
    val uses = state.everythingLocal.filter(_.props.contains(propId))
    uses.partition(_.isModel) match {
      case (m, i) => {
        models = m
        instances = i
      }
      case _ => failWith(s"Error partitioning Property uses for $propId")
    }
    
    for {
      dummy1 <- removeFrom(instances)
      dummy2 <- removeFrom(models)
      dummy3 = phaseDescription = s"Deleting Property $propName"
      dummy4 <- router.request(DeleteThing(rc, state.id, propId))
    }
      finished = true
  }
  
  private def removeFrom(things:Iterable[Thing]):RequestM[Unit] = {
    things.headOption match {
      case Some(thing) => {
        phaseDescription = s"Removing $propName from ${thing.displayName}"
        // Filter this Property out of InstanceProps if necessary:
        val instancePropsMap:PropMap =
          InstanceProps.fromOpt(thing.props) match {
            case Some(qv) => {
              // There are InstanceProps, so filter out this Property:
              val instanceProps = qv.filter(LinkType, { link:OID => link != propId })
              Map(InstanceProps(instanceProps))
            } 
            case None => Map.empty
          }
        router.request(ChangeProps(rc, state.id, thing.id, instancePropsMap ++ Map((propId -> DataModelAccess.getDeletedValue(prop))))) flatMap {
          case ThingFound(tid, newState) => {
            nDone = nDone + 1
            removeFrom(things.tail)
          }
          case ThingError(ex, _) => RequestM.failed(ex)
        }
      }
      
      case None => RequestM.successful(())
    }
  }
}

object RemovePropertyActor {
  def props(rc: RequestContext, propId:OID, ecology:Ecology, state:SpaceState, router:ActorRef) =
    Props(classOf[RemovePropertyActor], rc, propId, ecology, state, router)
}
