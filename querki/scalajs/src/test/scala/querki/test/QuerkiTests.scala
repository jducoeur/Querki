package querki.test

import upickle._
import utest._

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._

import querki.globals._

import querki.comm._
import querki.data._
import querki.ecology._
import querki.pages.ThingPageDetails

trait QuerkiTests extends TestSuite with EcologyMember {
  var ecology:EcologyImpl = null

  def setupEcology() = {
    ecology = new EcologyImpl
    createEcots(ecology)
    setupStandardEntryPoints()
    ecology.init(ClientState()) { state => state }
  }
  
  var _commStub:Option[ApiCommStub] = None
  def commStub = _commStub.get
  
  def createEcots(ecology:Ecology) = {
    _commStub = Some(new ApiCommStub(ecology))
    
    new querki.client.ClientImpl(ecology)
    new querki.data.ClientDataEcot(ecology)
    new querki.display.PageManagerEcot(ecology)
    new querki.display.input.InputGadgetsEcot(ecology)
    new querki.identity.UserManagerEcot(ecology)
    new querki.pages.PagesEcot(ecology)
  }

  def rawEntryPoint0(name:String)() = {
    lit(
      url = s"/test/$name"
    )
  }
  def entryPoint0(name:String)(userName:String, spaceId:String) = {
    lit(
      url = s"/test/$userName/$spaceId/$name"
    )
  }
  def entryPoint1(name:String)(userName:String, spaceId:String, p1:String) = {
    lit(
      url = s"/test/$userName/$spaceId/$name/$p1",
      
      // This is returning a JQueryDeferred, essentially.
      // TODO: make this pluggable!
      ajax = { () =>
        lit(
          done = { (cb:js.Function3[String, String, JQueryDeferred, Any]) =>
            // Note that we actually call the callback, and thus fulfill the Future in PlayAjax, synchronously.
            // This is an inaccuracy that could lead to us missing some bugs, but does make the testing more
            // deterministic.
            // TODO: enhance the framework to fire the callbacks asynchronously. We'll have to make sure the
            // Javascript framework doesn't exit prematurely, though, and the test will have to wait for results.
            cb(s"I am the message from calling $name", "", lit().asInstanceOf[JQueryDeferred])
          },
          fail = { (cb:js.Function3[JQueryDeferred, String, String, Any]) =>
            // We don't do anything here -- we're not failing for now.
            // TODO: we should do some fail tests!
          }
        ) 
      }
    )
  }
  
  def setupStandardEntryPoints() = {
    def controllers = commStub.controllers
    
    // Entry points referenced in the MenuBar, so need to be present in essentially every Page:
    controllers.Application.createProperty = { entryPoint0("createProperty") _ }
    controllers.Application.editThing = { entryPoint1("editThing") _ }
    controllers.Application.sharing = { entryPoint0("sharing") _ }
    controllers.Application.showAdvancedCommands = { entryPoint1("showAdvancedCommands") _ }
    controllers.Application.thing = { entryPoint1("thing") _ }
    controllers.Application.viewThing = { entryPoint1("viewThing") _ }
    
    controllers.ExploreController.showExplorer = { entryPoint1("showExplorer") _ }
    
    controllers.AdminController.manageUsers = { rawEntryPoint0("manageUsers") _ }
    controllers.AdminController.showSpaceStatus = { rawEntryPoint0("showSpaceStatus") _ }
    controllers.AdminController.sendSystemMessage = { rawEntryPoint0("sendSystemMessage") _ }
    
    controllers.TOSController.showTOS = { rawEntryPoint0("showTOS") _ }
    
    controllers.ClientController.apiRequest = { entryPoint1("apiRequest") _ }
  }
}

trait ThingPageTests extends QuerkiTests {
  
  lazy val DataSetting = interface[querki.data.DataSetting]
  lazy val PageManager = interface[querki.display.PageManager]
  
  /**
   * The default UserInfo. Tests may override this if necessary.
   */
  def userInfo = UserInfo(".userOid", Seq(IdentityInfo(".identityOid", "Test User", "testUser1")))
  
  /**
   * The default SpaceInfo. Tests may override this if necessary.
   */
  def spaceInfo = SpaceInfo(".spaceOid", Some("Test-Space"), "Test Space", "testUser1")
  
  /**
   * A very simple base Thing.
   */
  def thing1 = ThingInfo(".thingOid", Some("My-Thing"), "My Thing", ".modelOid", models.Kind.Thing, false, true, true, false, false)
  
  /**
   * A very simple Model.
   */
  def model1 = ThingInfo(".modelOId", Some("My-Model"), "My Model", ".simpleThing", models.Kind.Thing, true, false, false, true, false)

  /**
   * The RequestInfo for a very generic ThingPage.
   */
  def requestInfo = RequestInfo(Some(userInfo), Some(spaceInfo), Some(thing1), Seq(model1), false, false, ThingPageDetails(None))
  
  def setup[Output <: dom.Element](pageContent:scalatags.JsDom.TypedTag[Output]) = {
    // First, boot the system itself. This is more or less what happens in QuerkiClient:
    setupEcology()
    
    // Stuff the guts of the page into the body:
    val body = $("body").get(0).asInstanceOf[dom.Element]
    val contents = pageContent.render
    $(contents).appendTo(body)
    
    // Set things up. This stuff normally happens in client.scala.html:
    PageManager.setRoot(body)
    PageManager.setImagePath("/")
    
    val pickledRequest = write(requestInfo)
    DataSetting.unpickleRequest(pickledRequest)
    PageManager.renderPage(querki.pages.PageIDs.ThingPage, "")
  }
}
