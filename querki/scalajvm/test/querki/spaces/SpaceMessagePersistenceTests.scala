package querki.spaces

import models.{Kind, Thing}

import querki.globals._
import querki.persistence._
import querki.spaces.messages._
import querki.test._

class SpaceMessagePersistenceTests(env:PersistEnv) extends PersistTest(env) with SpaceMessagePersistenceBase {
  
  val s = env.commonSpace
  def state = s.state
  
  checkSerialization(querki.identity.IdentityPersistence.UserRef(s.owner.id, Some(s.owner.mainIdentity.id)))
  
  def checkDH[T <: UseKryo](builder: => T) = {
    checkSerialization(builder)
  }  
  
  /**
   * Check the raw serialization of the persisted Space messages.
   */
  checkDH(dh(CreateThing(s.owner, state.id, Kind.Thing, s.testModel.id, s.instance.props)))
  checkDH(dh(ModifyThing(s.member2.user, state.id, s.instance.id, s.testModel.id, Thing.emptyProps)))
  checkDH(dh(ChangeProps(s.owner, state.id, s.instance.id, s.instance.props, true)))
  checkDH(dh(DeleteThing(s.owner, state.id, s.instance.id)))
}
