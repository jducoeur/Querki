package querki.uservalues

import models.ModelPersistence._

import querki.globals._
import querki.identity.IdentityId
import querki.persistence._
import querki.time.DateTime

trait PersistentEvents {
  import PersistentEvents._
  import PersistMessages._
  
  def dh(uv:OneUserValue)(implicit state:SpaceState):DHUserValue = {
    val prop = state.prop(uv.propId).getOrElse(throw new Exception("SaveUserValue is trying to serialize unknown Property " + uv.propId))
    DHUserValue(
      uv.identity.id,
      uv.thingId,
      uv.propId,
      prop.serialize(uv.v),
      uv.modTime
    )
  }
}

object PersistentEvents {
  case class DHUserValue(
    @KryoTag(1) identityId:IdentityId,
    @KryoTag(2) thingId:OID,
    @KryoTag(3) propId:OID,
    @KryoTag(4) v:String,
    @KryoTag(5) modTime:DateTime
  ) extends UseKryo
}
