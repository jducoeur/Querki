package querki.uservalues

import models._

import querki.globals._
import querki.identity.IdentityId
import querki.persistence._
import querki.time.DateTime

trait PersistentEvents extends ModelPersistence { self: EcologyMember with querki.types.ModelTypeDefiner =>
  import PersistentEvents._

  def dh(
    identityId: IdentityId,
    thingId: OID,
    props: PropMap
  )(implicit
    state: SpaceState
  ) =
    DHUserValue(
      identityId,
      thingId,
      props,
      DateTime.now
    )
}

object PersistentEvents {
  import ModelPersistence._

  case class DHUserValue(
    @KryoTag(1) identityId: IdentityId,
    @KryoTag(2) thingId: OID,
    @KryoTag(3) props: DHPropMap,
    @KryoTag(4) modTime: DateTime
  ) extends UseKryo
}
