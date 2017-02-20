package models

import querki.ecology._
import querki.globals._
import querki.identity.IdentityId
import querki.persistence._
import querki.spaces.UnresolvedPropValue
import querki.time.DateTime
import querki.types.ModelTypeDefiner
import querki.values.SpaceState

/**
 * The Ecot for the Models. This is mainly responsible for dealing with serialization.
 */
class ModelEcot(e:Ecology) extends QuerkiEcot(e) {
  import ModelPersistence._
  
  override def persistentMessages = persist(64,
    (classOf[DHPropMap] -> 100),
    (classOf[DHThingState] -> 101),
    (classOf[DHProperty] -> 102),
    (classOf[DHModelType] -> 103),
    (classOf[DHSpaceState] -> 104)
  )
}
