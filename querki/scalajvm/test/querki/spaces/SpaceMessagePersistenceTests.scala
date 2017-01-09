package querki.spaces

import models.{Kind, Thing}

import querki.globals._
import querki.persistence._
import querki.spaces.messages._
import querki.test._
import querki.time.DateTime

import SpaceMessagePersistence._

class SpaceMessagePersistenceTests(env:PersistEnv) extends PersistTest(env) with SpaceMessagePersistenceBase {
  
  val s = env.commonSpace
  implicit val state = s.state
  
  checkSerialization(querki.identity.IdentityPersistence.UserRef(s.owner.id, Some(s.owner.mainIdentity.id)))
  
  def checkDH[T <: UseKryo](builder: => T) = {
    checkSerialization(builder)
  }  
  
  /**
   * Check the raw serialization of the persisted Space messages.
   */
  val time = DateTime.now
  val nextId = s.toid()
  checkDH(DHCreateThing(s.owner, nextId, Kind.Thing, s.testModel.id, s.instance.props, time, false))
  checkDH(DHModifyThing(s.member2.user, s.instance.id, Some(s.testModel.id), Thing.emptyProps, true, time))
  checkDH(DHDeleteThing(s.owner, s.instance.id, time))
}
