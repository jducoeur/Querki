package querki.pages

import upickle.default.{macroRW, ReadWriter => RW}

import models.Wikitext

import querki.search.SearchFunctions.SearchResults
import querki.data.ThingInfo

sealed trait PageDetails

case class ThingPageDetails(
  thingInfo: ThingInfo,
  modelInfo: Option[ThingInfo],
  customHeader: Option[Wikitext],
  rendered: Wikitext,
  stylesheets: Seq[String],
  headers: Seq[String]
) extends PageDetails

object ThingPageDetails {
  implicit val rw: RW[ThingPageDetails] = macroRW
}

case class SearchPageDetails(results: Option[SearchResults]) extends PageDetails
