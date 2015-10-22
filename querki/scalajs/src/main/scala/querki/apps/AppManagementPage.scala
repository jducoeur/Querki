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
class AppManagementPage(params:ParamMap)(implicit e:Ecology) extends Page(e) with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  
  val appInput = GadgetRef[RxText]
  
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
          for (app <- apps)
            yield p(b(IndexPage.spaceLink(app))),
          
          h3("Add an App"),
          p("Specify the OID of the App here, and press the button"),
          appInput <= new RxText(cls:="form-control col-md-3"),
          " ", 
          new ButtonGadget(ButtonGadget.Warning, "Add App", disabled := Rx { appInput.map(_.length == 0).getOrElse(true) }) ({ () =>
            Client[AppsFunctions].addApp(appInput.get.text()).call() foreach { success =>
              // Do something if this succeeded
            }
          })
        )
    }
      yield PageContents("App Management", guts)
  }
}
