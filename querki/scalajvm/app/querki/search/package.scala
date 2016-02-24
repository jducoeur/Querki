package querki

import querki.ecology._
import querki.globals._
import querki.values.{RequestContext, SpaceState}

package object search {
  trait Search extends EcologyInterface {
    def search(
      searchStr:String, 
      searchTags:Boolean = true, 
      searchThings:Boolean = true,
      modelId:Option[OID] = None,
      propertyId:Option[OID] = None)(implicit state:SpaceState):Option[SearchResultsInternal]
  }
}
