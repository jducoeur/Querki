package querki.pages

sealed trait PageDetails

case class ThingPageDetails(customHeader:Option[String]) extends PageDetails
