package querki.client

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.JSApp

import org.scalajs.dom

import querki.ecology._

/**
 * The root Querki Client. This is the main program that runs inside the browser, and renders
 * Querki pages in the standard modern-browser UI. It is not necessarily the be-all and end-all
 * of Querki rendering -- we're likely to have a highly dumbed-down version for older browsers,
 * and possibly some tuned apps
 */
object QuerkiClient extends JSApp with EcologyMember {
  
  /**
   * The top-level initializer for the application. This will be called first, when the page loads,
   * and should do all necessary setup.
   * 
   * Note that the actual page rendering happens when the outer page calls setRoot().
   */
  def main(): Unit = {
    setupEcology()
  }
  
  /**
   * The One True Pointer to the Ecology. NOTHING should use this pointer outside this class! Everything
   * else should receive their own Ecology pointers, and use that!
   */
  var ecology:EcologyImpl = null
  
  /**
   * Build and initialize the Ecology. Once this is complete, the world is running.
   */
  def setupEcology() = {
    ecology = new EcologyImpl
    createEcots(ecology)
    ecology.init(ClientState()) { state => state }
  }
  
  /**
   * Creates the Ecots that are used straightforwardly by most tests, as well as the running code.
   */
  def createCommonEcots(ecology:Ecology) = {
    new querki.admin.AdminEcot(ecology)
    new querki.client.ClientImpl(ecology)
    new querki.data.ClientDataEcot(ecology)
    new querki.datamodel.DataModelEcot(ecology)
    new querki.display.GadgetsEcot(ecology)
    new querki.display.PageManagerEcot(ecology)
    new querki.display.StatusLineEcot(ecology)    
    new querki.display.input.InputGadgetsEcot(ecology)
    new querki.editing.EditingEcot(ecology)
    new querki.identity.UserManagerEcot(ecology)
    new querki.notifications.NotificationsEcot(ecology)
    new querki.pages.PagesEcot(ecology)
    new querki.qtext.QTextUtilsEcot(ecology)
    new querki.search.SearchEcot(ecology)
    new querki.photos.PhotosEcot(ecology)
    new querki.print.PrintEcot(ecology)
  }

  /**
   * Create all of the Ecots. Every time a new one is created, it should be placed here.
   */
  def createEcots(ecology:Ecology) = {
    createCommonEcots(ecology)
    
    // List here any Ecots that usually need to be stubbed out for testing:
    new querki.comm.ApiCommEcot(ecology)
  }
  
  // Entry points, exposed for the Javascript layer:
  // TODO: not all of this is probably needed any more:
  @JSExport def dataSetting = interface[querki.data.DataSetting]
  @JSExport def pageManager = interface[querki.display.PageManager]
  @JSExport def userManager = interface[querki.identity.UserAccess]
}
