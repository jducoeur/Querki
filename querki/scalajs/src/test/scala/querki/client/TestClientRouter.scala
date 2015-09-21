package querki.client

import scala.concurrent.Future

import scala.scalajs.js
import org.querki.jquery._

import upickle._
import autowire._

import querki.api.{CommonFunctions, CommonFunctionsEmpty}
import querki.comm.URL
import querki.conversations.{ConversationFunctions, ConversationFunctionsEmpty, ConversationInfo}
import querki.globals._
import querki.notifications.{NotificationFunctions, NotificationFunctionsEmpty, NotificationInfo}

trait TestClientRouter {
  
  def commStub:querki.test.ApiCommStub
  
  trait AutowireHandler extends autowire.Server[String, upickle.Reader, upickle.Writer] {
    def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
    def write[Result: upickle.Writer](r: Result) = upickle.write(r)
    
    def handle(request:Core.Request[String]):Future[String]
  }

  var handlers = Map.empty[Seq[String], AutowireHandler]
  
  /**
   * Tests should call this to register the handlers that they expect to require.
   */
  def registerApiHandler[T](methods:String*)(handler:AutowireHandler)(implicit tag:scala.reflect.ClassTag[T]) = {
    val packageAndTrait = tag.runtimeClass.getName().split("\\.")
    val splitLocal = packageAndTrait.flatMap(_.split("\\$")).toSeq

    methods.foreach(method => handlers += ((splitLocal :+ method) -> handler))
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
      url = s"/test/$userName/$spaceId/$name/$p1"
    )
  }
  
  def ajaxEntryPointBase(name:String, url:String, handler:(String => Future[String])) = {
    lit(
      url = url,
      
      // This is returning a JQueryDeferred, essentially.
      ajax = { (settings:JQueryAjaxSettings) =>
        lit(
          done = { (cb:js.Function3[String, String, JQueryDeferred, Any]) =>
            val data = settings.data.asInstanceOf[String]
            val rawPairs = data.split("&")
            // NOTE: for the moment, we're only dealing with proper key=value pairs, since we really
            // only care about the pickledRequest:
            val splitPairs = rawPairs.map(_.split("=")).filter(_.length == 2)
            val pairs = splitPairs.map { pair =>
              (pair(0), js.URIUtils.decodeURIComponent(pair(1)))
            }
            val pickledRequest = pairs.find(_._1 == "pickledRequest") match {
              case Some(req) => req._2
              case None => throw new Exception("Didn't get a pickledRequest in an API call!")
            }
            
            // Note that we actually call the callback, and thus fulfill the Future in PlayAjax, synchronously.
            // This is an inaccuracy that could lead to us missing some bugs, but does make the testing more
            // deterministic.
            // TODO: enhance the framework to fire the callbacks asynchronously. We'll have to make sure the
            // Javascript framework doesn't exit prematurely, though, and the test will have to wait for results.
            val fut = handler(pickledRequest)
            fut.onFailure {
              case ex:Exception => println(s"Got async error $ex"); throw ex
            }
            fut.map { result => cb(result, "", lit().asInstanceOf[JQueryDeferred]) }
          },
          fail = { (cb:js.Function3[JQueryDeferred, String, String, Any]) =>
            // We don't do anything here -- we're not failing for now.
            // TODO: we should do some fail tests!
          }
        ) 
      },
	      
	  method = "PUT",
	  `type` = "PUT",
	  absoluteURL = s"http://www.querki.net$url"
    )
  }
  
  def ajaxCommonEntryPoint(name:String, handler:(String => Future[String]))() = {
    ajaxEntryPointBase(name, s"/test/$name", handler)
  }
  def ajaxApiEntryPoint(name:String, handler:(String => Future[String]))(userName:String, spaceId:String) = {
    ajaxEntryPointBase(name, s"/test/$userName/$spaceId/$name", handler)
  }
}

trait StandardTestEntryPoints extends TestClientRouter {
  
  def setupStandardEntryPoints() = {
    def controllers = commStub.controllers
    
    // ************************
    // Entry points referenced in the MenuBar, Footer, etc, so need to be present in essentially every Page:
    //
    controllers.AdminController.showSpaceStatus = { rawEntryPoint0("showSpaceStatus") _ }
    controllers.AdminController.sendSystemMessage = { rawEntryPoint0("sendSystemMessage") _ }
    
    controllers.TOSController.showTOS = { rawEntryPoint0("showTOS") _ }
    
    controllers.LoginController.userByName = { rawEntryPoint0("userByName") _ }
    controllers.LoginController.logout = { rawEntryPoint0("logout") _ }
    
    controllers.ClientController.space = { rawEntryPoint0("space") _ }
    
    // ************************
    // The main API handlers. Note that all handlers are currently placed in a common registry. I believe that should
    // be fine -- I don't see any way for them to conflict.
    //    
    def apiHandler(pickledRequest:String):Future[String] = {
      val request = read[Core.Request[String]](pickledRequest)
      handlers.get(request.path) match {
        case Some(handler) => handler.handle(request)
        case None => {
          val path = request.path.mkString(".")
          throw new Exception(s"Couldn't find registered handler for API call $path")
        }
      }      
    }
    controllers.ClientController.apiRequest = { ajaxApiEntryPoint("apiRequest", apiHandler) _ }
    controllers.ClientController.rawApiRequest = { ajaxApiEntryPoint("rawApiRequest", apiHandler) _ }
    controllers.ClientController.commonApiRequest = { ajaxCommonEntryPoint("commonApiRequest", apiHandler) _ }
    controllers.ClientController.userApiRequest = { ajaxCommonEntryPoint("userApiRequest", apiHandler) _ }
    
    registerApiHandler[CommonFunctions]("getStandardThings")(new CommonFunctionsEmpty with AutowireHandler {
      def handle(request:Core.Request[String]):Future[String] = route[CommonFunctions](this)(request) 
    })
    
    // We'll eventually want to allow conversation tests to override this, but for now let's define the standard
    // empty version:
    registerApiHandler[ConversationFunctions]("getConversationsFor")(new ConversationFunctionsEmpty with AutowireHandler {
      override def getConversationsFor(thingId:TID):Future[ConversationInfo] = {
        Future.successful(ConversationInfo(true, true, Seq.empty))
      }
      
      def handle(request:Core.Request[String]):Future[String] = route[ConversationFunctions](this)(request) 
    })
    
    // We'll eventually want to allow notification tests to override this, but for now let's define the standard
    // empty version:
    registerApiHandler[NotificationFunctions]("numNewNotifications")(new NotificationFunctionsEmpty with AutowireHandler {
      override def numNewNotifications():Int = {
        0
      }
      
      def handle(request:Core.Request[String]):Future[String] = route[NotificationFunctions](this)(request) 
    })
  }

}
