package querki.test

import scala.concurrent.Future

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

trait QuerkiTests extends TestSuite with EcologyMember with querki.client.TestClientRouter {
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
  
  def pageBody = $("body").get(0).asInstanceOf[dom.Element]
  
  def setup(bodyContents:Option[dom.Element] = None) = {
    // First, boot the system itself. This is more or less what happens in QuerkiClient:
    setupEcology()
    
    // Stuff the guts of the page into the body:
    bodyContents.map(contents => $(contents).appendTo(pageBody))
    
    // Set things up. This stuff normally happens in client.scala.html:
    PageManager.setRoot(pageBody)
    PageManager.setImagePath("/")
    
    val pickledRequest = write(requestInfo)
    DataSetting.unpickleRequest(pickledRequest)
  }

}

trait ThingPageTests extends QuerkiTests {
  
  def setupPage[Output <: dom.Element](pageContent:scalatags.JsDom.TypedTag[Output]) = {
    setup(Some(pageContent.render))
    
    PageManager.renderPage(querki.pages.PageIDs.ThingPage, "")
  }
}
