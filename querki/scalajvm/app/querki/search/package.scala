package querki

import querki.globals._
import querki.values.{SpaceState}

package object search {

  trait Search extends EcologyInterface {

    def search(
      searchStr: String,
      searchTags: Boolean = true,
      searchThings: Boolean = true,
      modelId: Seq[OID] = Seq.empty,
      propertyId: Seq[OID] = Seq.empty
    )(implicit
      state: SpaceState
    ): Option[SearchResultsInternal]
  }
}
