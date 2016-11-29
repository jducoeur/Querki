package querki.pages

import scalatags.JsDom.all._
import autowire._

import models.Wikitext
import querki.api.ThingFunctions
import querki.display.QText
import querki.globals._

class InfoPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {
  
  lazy val Apps = interface[querki.apps.Apps]
  lazy val Client = interface[querki.client.Client]
  lazy val SkillLevel = interface[querki.identity.skilllevel.SkillLevel]
  
  // In theory, you can only get to this Page if you're in a Space.
  // TODO: we really should have a way of formalizing that. The Space might actually
  // be a *parameter* to this Page instead?
  lazy val spaceInfo = DataAccess.space.get
  
  def pageContent = {
    val summaryFut = Client[ThingFunctions].getPropertyDisplay(spaceInfo.oid, std.apps.summaryProp.oid).call()
    val detailsFut = Client[ThingFunctions].getPropertyDisplay(spaceInfo.oid, std.apps.detailsProp.oid).call()
    
    for {
      summaryOpt <- summaryFut
      summaryText = summaryOpt.getOrElse(Wikitext.empty)
      detailsOpt <- detailsFut
      detailsText = detailsOpt.getOrElse(Wikitext.empty)
      allowExtract = DataAccess.request.isOwner && (SkillLevel.current == SkillLevel.AdvancedComplexity)
      guts =
        div(
          h1(s"Info about ${spaceInfo.displayName} ", a(cls:="cancelButton btn btn-default", href:=thingUrl(spaceInfo), "Done")),
          
          p(b(new QText(summaryText))),
          new QText(detailsText),
          
          // Do we show the Apps section?
          if (allowExtract || !spaceInfo.apps.isEmpty) {
            MSeq(
              h2("Apps"),
              for (app <- spaceInfo.apps)
                yield QText(
                  s"""**${IndexPage.spaceLink(app)}**"""),
              
              if (allowExtract) {
                div(
                  a(
                    cls:="btn btn-default btn-xs btn-primary_noPrint", 
                    href:=Apps.extractAppFactory.pageUrl(),
                    "Extract an App from this Space")
                )
              }
            )
          }
        )
    }
      yield PageContents(s"Info about ${spaceInfo.displayName}", guts)
  }
}
