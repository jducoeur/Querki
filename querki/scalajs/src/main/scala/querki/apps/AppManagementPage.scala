package querki.apps

import scalatags.JsDom.all._
import autowire._
import rx._

import querki.display.ButtonGadget
import querki.display.rx._
import querki.globals._
import querki.pages.{IndexPage, Page, PageContents, ParamMap}

/**
 * @author jducoeur
 */
class AppManagementPage(params:ParamMap)(implicit val ecology:Ecology) extends Page() {
  
  lazy val Apps = interface[Apps]
  lazy val Client = interface[querki.client.Client]
  
  val appInput = GadgetRef[RxText]
  
  def pageContent = {
    val content = DataAccess.space match {
      case Some(space) => {
        val apps = space.apps
        div(
          h1("App Management"),
          p("This page lets you add Apps to your Space. ", 
            b("Important:"), 
            " This feature is highly experimental, and not yet intended for general use!"),
            
          h3("Current Apps"),
          for (app <- apps)
            yield p(b(IndexPage.spaceLink(app))),
          
          h3("Add an App"),
          p("Specify the OID of the App here, and press the button"),
          appInput <= new RxText(cls:="form-control col-md-3"),
          " ", 
          new ButtonGadget(ButtonGadget.Warning, "Add App", disabled := Rx { appInput.map(_.length == 0).getOrElse(true) }) ({ () =>
            Client[AppsFunctions].addApp(appInput.get.text()).call() foreach { success =>
              // Things may have changed a *lot*, so do a complete reload:
              PageManager.fullReload()
            }
          })
        )
      }
      case None => {
        div(h1("ERROR: this page should not be available outside of a Space"))
      }
    }
    Future.successful(PageContents("App Management", content))
  }
}
