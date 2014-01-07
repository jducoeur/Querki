package querki

import querki.ecology._

import querki.values.RequestContext

package object search {
  trait Search extends EcologyInterface {
    def search(rc:RequestContext, searchStr:String):Option[SearchResults]
  }
}