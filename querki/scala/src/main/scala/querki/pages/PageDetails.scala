package querki.pages

import models.Wikitext

import querki.api.SearchFunctions.SearchResults
import querki.data.ThingInfo

sealed trait PageDetails

case class ThingPageDetails(thingInfo:ThingInfo, modelInfo:Option[ThingInfo], customHeader:Option[Wikitext], rendered:Wikitext) extends PageDetails
case class SearchPageDetails(results:Option[SearchResults]) extends PageDetails
