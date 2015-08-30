package querki.pages

import models.Wikitext

import querki.search.SearchFunctions.SearchResults
import querki.data.ThingInfo

sealed trait PageDetails

case class ThingPageDetails(
  thingInfo:ThingInfo, 
  modelInfo:Option[ThingInfo], 
  customHeader:Option[Wikitext], 
  rendered:Wikitext, 
  stylesheets:Seq[String],
  headers:Seq[String]
) extends PageDetails
case class SearchPageDetails(results:Option[SearchResults]) extends PageDetails
