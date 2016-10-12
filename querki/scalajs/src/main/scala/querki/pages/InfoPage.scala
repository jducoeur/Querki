package querki.pages

import scalatags.JsDom.all._
import autowire._

import models.Wikitext
import querki.api.ThingFunctions
import querki.display.QText
import querki.globals._

class InfoPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  
  // In theory, you can only get to this Page if you're in a Space.
  // TODO: we really should have a way of formalizing that. The Space might actually
  // be a *parameter* to this Page instead?
  lazy val spaceInfo = DataAccess.space.get
  
  /* TODO: fill in the Apps buttons here, as appropriate. Below is the old menu, which is being
   * replaced here:
  def appsSection = 
    if (!hasExplore)
      None
    else
      spaceOpt.map { space =>
        NavSection("Apps", Seq(
          NavLink("Get this App", enabled = space.permissions.contains(std.apps.canUseAsAppPerm)),
          NavLink(
            "Manage Apps", 
            Apps.appMgmtFactory.pageUrl(), 
            enabled = space.permissions.contains(std.apps.canManipulateAppsPerm), 
            complexity = Advanced),
          NavLink(
            "Extract an App", 
            Apps.extractAppFactory.pageUrl(), 
            enabled = space.permissions.contains(std.apps.canManipulateAppsPerm),
            complexity = Standard)
        ), 1200)
    }
   */
  
  def pageContent = {
    val summaryFut = Client[ThingFunctions].getPropertyDisplay(spaceInfo.oid, std.conventions.summaryProp.oid).call()
    val detailsFut = Client[ThingFunctions].getPropertyDisplay(spaceInfo.oid, std.conventions.detailsProp.oid).call()
    
    for {
      summaryOpt <- summaryFut
      summaryText = summaryOpt.getOrElse(Wikitext.empty)
      detailsOpt <- detailsFut
      detailsText = detailsOpt.getOrElse(Wikitext.empty)
      guts =
        div(
          h1(s"Info about ${spaceInfo.displayName} ", a(cls:="cancelButton btn btn-default", href:=thingUrl(spaceInfo), "Done")),
          p(b(new QText(summaryText))),
          new QText(detailsText)
        )
    }
      yield PageContents(s"Info about ${spaceInfo.displayName}", guts)
  }
}
