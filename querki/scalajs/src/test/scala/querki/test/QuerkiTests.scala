package querki.test

import scala.concurrent.{Future, Promise}

import scala.async.Async._

import upickle._
import utest._
import autowire._

import scala.scalajs.js
import org.scalajs.dom
import dom.html
import org.querki.jquery._
import org.querki.jsext._
import scalatags.JsDom.all._

import querki.globals._

import querki.client.QuerkiClient
import querki.comm._
import querki.data.{TID => TIDdummy, _}
import querki.ecology._
import querki.identity.UserLevel
import querki.pages.{Page, ThingPageDetails}

trait QuerkiTests extends TestSuite with EcologyMember with querki.client.StandardTestEntryPoints {
  var ecology:EcologyImpl = null

  def setupEcology(addEntryPointsCb:Option[js.Dynamic => Unit] = None) = {
    ecology = new EcologyImpl
    createEcots(ecology)
    setupStandardEntryPoints()
    addEntryPointsCb.map(cb => cb(commStub.controllers))
    ecology.init(ClientState()) { state => state }
  }
  
  var _commStub:Option[ApiCommStub] = None
  def commStub = _commStub.get
  
  def createEcots(ecology:Ecology) = {
    QuerkiClient.createCommonEcots(ecology)
    
    _commStub = Some(new ApiCommStub(ecology))
  }
  
  /**
   * Note that these must be defs, not lazy vals, so that they don't persist across different tests!
   * 
   * This is a general rule: if you reference interfaces at the class level, don't use lazy val!
   */
  def DataSetting = interface[querki.data.DataSetting]
  def PageManager = interface[querki.display.PageManager]
  
  /**
   * The default UserInfo. Tests may override this if necessary.
   */
  def userInfo = UserInfo(".userOid", Seq(IdentityInfo(".identityOid", "Test User", "testUser1")))
  
  /**
   * The default SpaceInfo. Tests may override this if necessary.
   */
  def spaceInfo = SpaceInfo(TID(".spaceOid"), Some("Test-Space"), "Test Space", ".testUser1", "testUser1")
  
  /**
   * A very simple base Thing.
   */
  def thing1 = ThingInfo(TID(".thingOid"), Some("My-Thing"), "My Thing", TID(".modelOid"), models.Kind.Thing, false, true, true, false, false, None)
  
  /**
   * A very simple Model.
   */
  def model1 = ThingInfo(TID(".modelOId"), Some("My-Model"), "My Model", TID(".simpleThing"), models.Kind.Thing, true, false, false, true, false, None)

  /**
   * The RequestInfo for a very generic ThingPage.
   */
  def requestInfo = RequestInfo(Some(userInfo), Some(spaceInfo), false, UserLevel.FreeUser)
  
  def pageBody = $("body").get(0).asInstanceOf[dom.html.Body]
  
  def setup(bodyContents:Option[dom.Element] = None, addEntryPointsCb:Option[js.Dynamic => Unit] = None) = {
    // First, boot the system itself. This is more or less what happens in QuerkiClient:
    setupEcology(addEntryPointsCb)
    
    // Stuff the guts of the page into the body:
    bodyContents.map(contents => $(contents).appendTo(pageBody))
    
    // Set things up. This stuff normally happens in client.scala.html:
    PageManager.setImagePath("/")
    
    val pickledRequest = write(requestInfo)
    DataSetting.unpickleRequest(pickledRequest)
  }

  /**
   * This will execute the given trigger code (which should cause a Page change), 
   * and returns a Future that will be fulfilled once the resulting Page is fully
   * loaded. 
   */
  def afterPageChange(trigger: => Unit):Future[html.Div] = {
    val promise = Promise[html.Div]
    PageManager.nextChangeFuture.map { page =>
      page.renderedContentFuture.map { content =>
        promise.success(content)
      }
    }
    trigger
    promise.future
  }
}

trait ThingPageTests extends QuerkiTests {
  
  import models.Wikitext
  import querki.api._
  
  def pageName = thing1.linkName.get
  
  def setupPage[Output <: dom.Element](
    pageContent:scalatags.JsDom.TypedTag[Output],
    addEntryPointsCb:Option[js.Dynamic => Unit] = None):Future[Page] = 
  {
    setup(addEntryPointsCb = addEntryPointsCb)
    
    val renderedGuts = pageContent.toString
    
    registerApiHandler[ThingFunctions]("getThingPage")(new ThingFunctionsEmpty with AutowireHandler {
      override def getThingPage(thingId:TID):ThingPageDetails = {
        ThingPageDetails(
          thing1,
          Some(model1),
          None,
          Wikitext(renderedGuts),
          Seq.empty,
          Seq.empty
        )
      }
    
      def handle(request:Core.Request[String]):Future[String] = route[ThingFunctions](this)(request)
    })
    
    // TODO: once my PR for scala-js-dom gets released, making ownerDocument an HTMLDocument in this
    // context, replace this with the commented-out version:
    val window = pageBody.ownerDocument.asInstanceOf[html.Document].defaultView
//    val window = pageBody.ownerDocument.defaultView
    
    window.location.hash = "#" + pageName
    // This will cause the page to render, based on the current hash:
    // This triggers an exception if something went wrong during page rendering:
    PageManager.setRoot(window, pageBody).withTimeout("Page failed to render")
  }
}
