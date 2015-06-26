package querki

import querki.ecology._

import querki.values.{RequestContext, SpaceState}

package object search {
  trait Search extends EcologyInterface {
    def search(searchStr:String)(implicit state:SpaceState):Option[SearchResultsInternal]
  }
}