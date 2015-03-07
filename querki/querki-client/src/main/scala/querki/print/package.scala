package querki

import querki.globals._

import querki.data.ThingInfo

package object print {
  trait Print extends EcologyInterface {
    def print(thing:ThingInfo):Unit
  }
}
