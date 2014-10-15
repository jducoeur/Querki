package querki.pages

import models.Wikitext

import querki.api.SearchFunctions.SearchResults

sealed trait PageDetails

case class ThingPageDetails(customHeader:Option[Wikitext]) extends PageDetails
case class SearchPageDetails(results:Option[SearchResults]) extends PageDetails
