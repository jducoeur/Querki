package querki.apps

import scalatags.JsDom.all._
import autowire._

import querki.globals._
import querki.pages.{Page, PageContents, ParamMap}

/**
 * @author jducoeur
 */
class AppManagementPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  
  def pageContent = {
    for {
      apps <- Client[AppsFunctions].getApps().call()
      guts =
        div(
          h1("App Management"),
          
          p("This page lets you add Apps to your Space. ", 
            b("Important:"), 
            " This feature is highly experimental, and not yet intended for general use!"),
            
          h3("Current Apps"),
          
          for (app <- apps) {
            p(app.displayName)
          }
        )
    }
      yield PageContents("Manager Apps", guts)
  }
}
