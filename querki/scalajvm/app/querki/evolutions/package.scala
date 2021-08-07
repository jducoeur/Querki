package querki

import models.OID

import querki.ecology._
import querki.identity.UserId

package object evolutions {

  trait Evolutions extends EcologyInterface {

    def checkEvolution(
      spaceId: OID,
      version: Int
    ): Unit
  }

  trait UserEvolutions extends EcologyInterface {

    def checkUserEvolution(
      userId: UserId,
      version: Int
    ): Unit
  }
}
