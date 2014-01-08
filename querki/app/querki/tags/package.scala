package querki

import querki.ecology._

import models.Property

import querki.values.SpaceState

package object tags {
  trait Tags extends EcologyInterface {
    def fetchTags(space:SpaceState, propIn:Property[_,_]):Set[String]
  }
}