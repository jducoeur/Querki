package querki.spaces

import akka.persistence.PersistentActor

import models.{ModelPersistence, UnknownOID, ThingId}
import models.Kind.Kind
import ModelPersistence._

import querki.globals._
import querki.identity.{User, IdentityPersistence}
import IdentityPersistence._
import querki.persistence._
import querki.time.DateTime

import messages._

/**
 * This defines the "dehydrated" serializable forms of the SpaceMessages. These are the commands we
 * actually process, with sufficient information to replay them at load time.
 * 
 * Note that these versions are intentionally stripped-down. For example, they don't contain the Space ID,
 * because that's redundant for our purposes: the message is being persisted in this Space's archive. They
 * turn out not to quite match the public messages, and that's deliberate: the fields being persisted are
 * the ones that are there after we do all the checks, which are needed to make the real state changes.
 */
object SpaceMessagePersistence {
  case class DHCreateThing(
    @KryoTag(1) req:UserRef, 
    @KryoTag(2) id:OID,
    @KryoTag(3) kind:Kind, 
    @KryoTag(4) modelId:OID, 
    @KryoTag(5) props:DHPropMap, 
    @KryoTag(6) modTime:DateTime) extends UseKryo
    
  case class DHModifyThing(
    @KryoTag(1) req:UserRef, 
    @KryoTag(2) id:OID, 
    @KryoTag(3) modelIdOpt:Option[OID], 
    @KryoTag(4) propChanges:DHPropMap,
    @KryoTag(5) replaceAllProps:Boolean,
    @KryoTag(6) modTime:DateTime) extends UseKryo
    
  case class DHDeleteThing(
    @KryoTag(1) req:UserRef, 
    @KryoTag(2) id:OID, 
    @KryoTag(3) modTime:DateTime) extends UseKryo
    
  case class DHInitState(
    @KryoTag(1) req:UserRef,
    @KryoTag(2) display:String) extends UseKryo
    
  case class SpaceSnapshot(
    @KryoTag(1) state:DHSpaceState) extends UseKryo
}

trait SpaceMessagePersistenceBase extends EcologyMember with ModelPersistence with querki.types.ModelTypeDefiner {
  import SpaceMessagePersistence._
  
  private lazy val Person = interface[querki.identity.Person]
  implicit def user2Ref(user:User)(implicit state:SpaceState):UserRef = Person.user2Ref(user)
}
