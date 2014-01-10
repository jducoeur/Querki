package querki

import querki.ecology._

import models.OID

package object evolutions {
  trait Evolutions extends EcologyInterface {
    def checkEvolution(spaceId:OID, version:Int):Unit
  }
}