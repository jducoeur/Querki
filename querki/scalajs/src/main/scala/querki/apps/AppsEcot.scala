package querki.apps

import org.scalajs.dom
import scalatags.JsDom.all._
import autowire._

import org.querki.gadgets._
import org.querki.jquery._

import querki.display.{ButtonGadget, Dialog, HookedGadget}
import querki.display.rx.RxInput
import querki.ecology._
import querki.globals._
import querki.pages.CreateSpacePage
import querki.session.UserFunctions
import querki.util.InputUtils

/**
 * @author jducoeur
 */
class AppsEcot(e:Ecology) extends ClientEcot(e) with Apps {
  def implements = Set(classOf[Apps])
  
  lazy val Client = interface[querki.client.Client]
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val Gadgets = interface[querki.display.Gadgets]
  lazy val Pages = interface[querki.pages.Pages]
  lazy val UserAccess = interface[querki.identity.UserAccess]
  
  lazy val appMgmtFactory = Pages.registerStandardFactory("_appMgmt", { (params) => new AppManagementPage(params) })
  lazy val extractAppFactory = Pages.registerStandardFactory("_extractApp", { (params) => new ExtractAppPage(params) })
  
  override def postInit() = {
    appMgmtFactory
    extractAppFactory
    
    Gadgets.registerSimpleGadget("._instantiateAppButton", { new UseAppGadget })
  }
    
  lazy val spaceInfo = DataAccess.space.get
  lazy val isApp = spaceInfo.isApp
  
  /**
   * A simple hook, so that anything with the class "_instantiateAppButton" becomes a button to pop the dialog.
   */
  class UseAppGadget extends HookedGadget[dom.html.Element](ecology) {
    def doRender = ???
    
    def hook() = {
      $(elem).click { evt:JQueryEventObject =>
        useApp()
      }
    }
  }
  
  /**
   * The guts of useApp(), which expect that we have a logged-in user.
   */
  private def doUseApp() = {
    val spaceName = GadgetRef[RxInput]
    spaceName.whenSet { g => 
      g.onEnter { text =>
        if (text.length() > 0) {
          createSpace()
        }
      }
    }
    
    def createSpace() = {
      val newName = spaceName.get.text().trim
      if (newName.length > 0) {
        Client[UserFunctions].createSpace(newName, Some(spaceInfo.oid)).call().map { newSpaceInfo =>
          CreateSpacePage.navigateToSpace(newSpaceInfo)
        }
      }
    }
    
    val confirmDialog = new Dialog(
      s"Create a Space using ${spaceInfo.displayName}",
      div(
        p(s"This will create a new Space, owned by you, using ${spaceInfo.displayName}. What should the Space be named?"),
        spaceName <= new RxInput(
            Some(InputUtils.spaceNameFilter _), "text", value := spaceInfo.displayName,
            id:="_newSpaceName", cls:="form-control", maxlength:=254, tabindex:=200)
      ),
      (ButtonGadget.Primary, Seq("Create", id := "_modelSelected"), { dialog =>
        createSpace
        dialog.done()
      }),
      (ButtonGadget.Normal, Seq("Cancel", id := "_modelCancel"), { dialog => 
        dialog.done() 
      })
    )
    
    confirmDialog.show()
  }
  
  def useApp() = {
    // Belt-and-suspenders check:
    if (isApp) {
      if (UserAccess.loggedIn)
        doUseApp()
      else
        // Log in first, *then* instantiate the App. Note that you can also join Querki
        // through this pathway, quite intentionally:
        UserAccess.loginCore().map(_ => doUseApp())
    }
  }
}
