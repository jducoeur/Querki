package querki.search

import models._

import querki.ecology._

import querki.identity.User

import querki.values.{RequestContext, SpaceState}

object MOIDs extends EcotIds(19)

class SearchEcot(e:Ecology) extends QuerkiEcot(e) with Search {
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  override def postInit() = {
    // Search does not require login:
    ApiRegistry.registerUserSessionImplFor[SearchFunctions, SearchFunctionsImpl](SpaceOps.spaceRegion, false)
  }
  
  lazy val DisplayNameProp = interface[querki.basic.Basic].DisplayNameProp
  
  def search(searchStr:String)(implicit state:SpaceState):Option[SearchResultsInternal] = {
    if (searchStr.length() < 3)
      None
    else {
      val searchComp = searchStr.toLowerCase()
      
      def checkOne(t:Thing):Seq[SearchResultInternal] = {
        if (t.unsafeDisplayName.toLowerCase().contains(searchComp)) {
          Seq(SearchResultInternal(t, DisplayNameProp, 1.0, t.unsafeDisplayName, List(t.unsafeDisplayName.toLowerCase().indexOf(searchComp))))
        } else {
          // For now, we're only going to deal with Text types.
          // TODO: cope with Links, Tags, and things like that!
          // TODO: this currently only takes the first elem from the QValue; it should work on
          // all of them!
          val tResults:Iterable[Option[SearchResultInternal]] = t.props.map { pair =>
            val (propId, propValue) = pair
            propValue.pType match {
              case textType:querki.core.TextTypeBasis#TextTypeBase => {
                val firstVal = propValue.firstAs(textType)
                firstVal.flatMap { qtext => 
                  val propComp = qtext.text.toLowerCase()
                  if (propComp.contains(searchComp)) {
                    
                    def matchLocs(start:Int):List[Int] = {
                      val i = propComp.indexOf(searchComp, start)
                      if (i == -1)
                        Nil
                      else
                        i :: matchLocs(i + 1)
                    }

                    Some(SearchResultInternal(t, state.prop(propId).get, 0.5, qtext.text, matchLocs(0)))
                  }
                  else
                    None
                }
              }
              case _ => None
            }
          }
          tResults.flatten.toSeq
        }
      }
      
      val allResults = state.everythingLocal.map(checkOne(_)).flatten.toSeq
    
      Some(SearchResultsInternal(searchStr, allResults))      
    }
  }
}