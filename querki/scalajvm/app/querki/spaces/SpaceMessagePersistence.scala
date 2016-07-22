package querki.spaces

import akka.persistence.PersistentActor

import models.{ModelPersistence, ThingId}
import models.Kind.Kind
import ModelPersistence._

import querki.globals._
import querki.identity.{User, IdentityPersistence}
import IdentityPersistence._
import querki.persistence._

import messages._

/**
 * This defines the "dehydrated" serializable forms of the SpaceMessages. These are the commands we
 * actually process, with sufficient information to replay them at load time.
 * 
 * Note that these versions are intentionally stripped-down. For example, they don't contain the Space ID,
 * because that's redundant for our purposes: the message is being persisted in this Space's archive.
 */
object SpaceMessagePersistence {
  case class DHCreateThing(@KryoTag(1) req:UserRef, @KryoTag(2) kind:Kind, @KryoTag(3) modelId:OID, @KryoTag(4) props:DHPropMap) extends UseKryo
  case class DHModifyThing(@KryoTag(1) req:UserRef, @KryoTag(2) id:ThingId, @KryoTag(3) modelId:OID, @KryoTag(4) props:DHPropMap) extends UseKryo
  case class DHChangeProps(@KryoTag(1) req:UserRef, @KryoTag(2) id:ThingId, @KryoTag(3) changedProps:DHPropMap) extends UseKryo
  case class DHDeleteThing(@KryoTag(1) req:UserRef, @KryoTag(2) thing:ThingId) extends UseKryo
}

trait SpaceMessagePersistenceBase extends EcologyMember with ModelPersistence with querki.types.ModelTypeDefiner {
  import SpaceMessagePersistence._
  
  private lazy val Person = interface[querki.identity.Person]
  implicit def user2Ref(user:User)(implicit state:SpaceState):UserRef = Person.user2Ref(user)
  
  def state:SpaceState
  
  def dh(msg:CreateThing) = { implicit val s = state; DHCreateThing(msg.req, msg.kind, msg.modelId, msg.props) }
  def dh(msg:ModifyThing) = { implicit val s = state; DHModifyThing(msg.req, msg.id, msg.modelId, msg.props) }
  def dh(msg:ChangeProps) = { implicit val s = state; DHChangeProps(msg.req, msg.id, msg.changedProps) }
  def dh(msg:DeleteThing) = { implicit val s = state; DHDeleteThing(msg.req, msg.thing) }
}

trait SpaceMessagePersistence extends SpaceMessagePersistenceBase with PersistentActor {
  
  /**
   * A slightly more strong-typed version of persist(), just for good hygiene.
   */
  def ps[A <: UseKryo](msg:A)(handler: (A) => Unit):Unit = {
    persist(msg)(handler)
  }
  
}