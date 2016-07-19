package querki.spaces

import models.{Kind, Thing}

import querki.globals._
import querki.persistence._
import querki.spaces.messages._
import querki.test._

class SpaceMessagePersistenceTests(env:PersistEnv) extends PersistTest(env) with SpaceMessagePersistenceBase {
  
  val s = env.commonSpace
  def ecology = env.ecology
  def state = s.state
  
  checkSerialization(querki.identity.IdentityPersistence.UserRef(s.owner.id, Some(s.owner.mainIdentity.id)))
  
  def checkDH[T <: UseKryo](builder: => T) = {
    checkSerialization(builder)
  }  
  
  checkDH(dh(CreateThing(s.owner, s.toid(), Kind.Thing, s.testModel.id, Thing.emptyProps)))
}
