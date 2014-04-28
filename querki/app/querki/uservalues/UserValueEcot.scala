package querki.uservalues

import akka.actor.Props

import models.{Kind, PType}

import querki.ecology._
import querki.spaces.{CacheUpdate, ThingChangeRequest}
import querki.util.{Contributor, Publisher, QLog}
import querki.values.{SpaceState, StateCacheKey}

object MOIDs extends EcotIds(44) {
  val RatingTypeOID = moid(1)
  val UserValuePermissionOID = moid(2)
  val IsUserValueFlagOID = moid(3)
}

class UserValueEcot(e:Ecology) extends QuerkiEcot(e) with UserValues {
  import MOIDs._
  
  val AccessControl = initRequires[querki.security.AccessControl]
  val SpaceChangeManager = initRequires[querki.spaces.SpaceChangeManager]
  
  override def init = {
    SpaceChangeManager.updateStateCache += UserValueCacheUpdater
//    SpaceChangeManager.thingChanges += UserValueUpdateFilter
  }
  
  override def term = {
//    SpaceChangeManager.thingChanges += UserValueUpdateFilter
    SpaceChangeManager.updateStateCache -= UserValueCacheUpdater    
  }
  
  object StateCacheKeys {
    val userValueProps = "UserValueProps"
  }
  
  private object UserValueCacheUpdater extends Contributor[CacheUpdate, CacheUpdate] {  
    /**
     * This gets called whenever a SpaceState is updated. Figure out all the UserValue Properties (if any),
     * so that we can process them efficiently.
     */
    def notify(evt:CacheUpdate, sender:Publisher[CacheUpdate, CacheUpdate]):CacheUpdate = {
      implicit val state = evt.current
      val uvPropPairs = state.allProps.filter { pair =>
        val (pid, prop) = pair
        prop.ifSet(IsUserValueFlag)
      }
      val calculated:Set[OID] = uvPropPairs.keys.toSet
        
      evt.updateCacheWith(MOIDs.ecotId, StateCacheKeys.userValueProps, calculated)
    }
  }
//  
//  private object UserValueUpdateFilter extends Contributor[ThingChangeRequest, ThingChangeRequest] {
//    // MASSIVE HACK
//    // This is called whenever we get a Create or Modify request. We look for UserValue Property values that aren't
//    // UserValueWrappers -- if so, they are actually userType values, and shouldn't be serialized here. We replace
//    // those with the existing summary values.
//    // NOTE: this is working hand-in-glove with HtmlRenderer.propValFromUser(), which always returns the *userType*
//    // rather than the wrapper. In legit cases, from _edit, this will become a ChangeProps message and will go through
//    // the UserSession, which will handle it. Otherwise, it'll go to the Space, and we will need to screen it out.
//    // Eventually, we may want to fix this horrible hack, but probably can't until *all* changes are going through
//    // UserSession, in which case we could always display the userType editor and always screen the changes in UserSession.
//    def notify(evt:ThingChangeRequest, sender:Publisher[ThingChangeRequest,ThingChangeRequest]):ThingChangeRequest = {
//      evt match {
//        case ThingChangeRequest(state, modelOpt, oldThing, props, changedProps) => {
//          changedProps.filter { isUserValueProp(_)(state) } match {
//            case Seq() => evt
//            case uvChanges:Seq[OID] => {
//              // Okay, some UserValue Properties have changed. Are the changes legit? Filter out any that aren't.
//              val fixedProps = (props /: uvChanges) { (curProps, propId) =>
//                curProps.get(propId) match {
//                  case Some(v) => {
//                    state.prop(propId) match {
//                      case Some(prop) => {
//                        if (v.pType.realType == prop.pType) {
//                          // Okay, this is an actual UserValueWrapper, so let it through:
//                          curProps
//                        } else {
//                          // The value doesn't actually match the type, so we will presume that it is userType;
//                          // in that case we will just replace it with the existing wrapper, if there is one:
//                          oldThing.flatMap(_.props.get(propId)) match {
//                            case Some(wrapper) => curProps + (propId -> wrapper)
//                            // TODO: this is a bug! Fix it, if this code survives at all!
//                            case None => curProps - propId
//                          }
//                        }
//                      }
//                      case None => {
//                        QLog.error("UserValueEcot.UserValueUpdateFilter got notified of changes to unknown property " + propId)
//                        curProps
//                      }
//                    }
//                  }
//                  // The property has been removed, so we don't care:
//                  case None => curProps
//                }
//              }
//              
//              ThingChangeRequest(state, modelOpt, oldThing, fixedProps, changedProps)
//            }
//          }
//        }
//        case _ => evt
//      }
//    }
//  }
  
  def isUserValueProp(propId:OID)(implicit state:SpaceState):Boolean = {
    state.cache.get(StateCacheKey(MOIDs.ecotId, StateCacheKeys.userValueProps)) match {
      case Some(rawEntry) => { rawEntry.asInstanceOf[Set[OID]].contains(propId) }
      case None => { QLog.error("UserValueEcot couldn't find its state cache in Space " + state.id); false }
    }    
  }
  
  def getUserType(pt:PType[_]):Option[PType[_]] = {
    // This really ought to work with a simple match, but it doesn't. Why not? I am mystified...
    if (pt.isInstanceOf[TUserValue])
      Some(pt.asInstanceOf[TUserValue].userType)
    else
      None
  }
  
  def wrapUserValue(uv:QValue, pt:PType[_], oldWrapperOpt:Option[QValue]):QValue = {
    if (pt.isInstanceOf[OldUserValueType[_,_]]) {
      pt.asInstanceOf[OldUserValueType[_,_]].wrapValue(uv, oldWrapperOpt)
    } else
      throw new Exception("UserValueEcot asked to wrap something in a PType that isn't OldUserValueType!")
  }
  
  // TODO: the following Props signature is now deprecated, and should be replaced (in Akka 2.2)
  // with "Props(classOf(Space), ...)". See:
  //   http://doc.akka.io/docs/akka/2.2.3/scala/actors.html
  def userValuePersisterProps(spaceId:OID):Props = 
    Props(new UserValuePersister(spaceId, ecology))
      
  /******************************************
   * TYPES
   ******************************************/
  
  class RatingType extends OldUserValueType[Int,DiscreteSummary[Int]](RatingTypeOID,
      toProps(
        setName("Rating Type")))
  {
    // TODO: is this really IntType, or is it a specialized subType? At the least, it needs to set
    // appropriate bounds on the values, and control the UI.
    val userType = Core.IntType
    
    val summarizer = new DiscreteSummarizer(userType)
  }
  lazy val RatingType = new RatingType
  
  override lazy val types = Seq(
    RatingType
  )
    
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val UserValuePermission = AccessControl.definePermission(
      UserValuePermissionOID, 
      "Who Can Have User Values", 
      "Who is allowed to define their own User Values (such as Ratings or Reviews)",
      Seq(AccessControl.OwnerTag, AccessControl.MembersTag), false)
      
  lazy val IsUserValueFlag = new SystemProperty(IsUserValueFlagOID, YesNoType, ExactlyOne,
      toProps(
        setName("Is User Value Property"),
        AppliesToKindProp(Kind.Property),
        SkillLevel(SkillLevelAdvanced),
        Summary("Add this flag to a Property, and set it to true, if this Property should have a separate value for each user.")))

  override lazy val props = Seq(
    UserValuePermission,
    IsUserValueFlag
  )
}