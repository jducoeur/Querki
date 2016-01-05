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
  lazy val Tags = interface[querki.tags.Tags]
  
  override def postInit() = {
    // Search does not require login:
    ApiRegistry.registerApiImplFor[SearchFunctions, SearchFunctionsImpl](SpaceOps.spaceRegion, false)
  }
  
  lazy val DisplayNameProp = interface[querki.basic.Basic].DisplayNameProp
  
  def search(searchStr:String)(implicit state:SpaceState):Option[SearchResultsInternal] = {
    if (searchStr.length() < 3)
      None
    else {
      val searchComp = searchStr.toLowerCase()
      
      def matchLocs(s:String):List[Int] = {
        def matchLocsRec(start:Int):List[Int] = {
          val i = s.indexOf(searchComp, start)
          if (i == -1)
            Nil
          else
            i :: matchLocsRec(i + 1)
        }
        
        matchLocsRec(0)
      }
      
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
                  if (propComp.contains(searchComp))
                    Some(SearchResultInternal(t, state.prop(propId).get, 0.5, qtext.text, matchLocs(propComp)))
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
      
      val thingResults = state.everythingLocal.map(checkOne(_)).flatten.toSeq
      val tagResults:Seq[SearchResultInternal] =
        Tags
          .fetchAllTags(state)
          .filter(_.toLowerCase.contains(searchComp))
          .map(Tags.getTag(_, state))
          .map(tag => SearchResultInternal(tag, DisplayNameProp, 0.7, tag.displayName, matchLocs(tag.displayName.toLowerCase)))
          .toSeq
    
      Some(SearchResultsInternal(searchStr, thingResults ++ tagResults))      
    }
  }
}
