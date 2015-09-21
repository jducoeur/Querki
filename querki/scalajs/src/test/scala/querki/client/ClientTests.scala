package querki.client

import scala.concurrent.Future

import scala.async.Async._

import utest._
import autowire._

import querki.comm._
import querki.globals._
import querki.test._

object ClientTests extends QuerkiTests {
  
  lazy val Client = interface[querki.client.Client]
  
  trait ClientTestTrait {
    def getMsg(partial:String):String
    def somethingElse():Int
    def another(x:Int):String
  }
  
  trait ClientTestEmpty extends ClientTestTrait {
    def getMsg(partial:String):String = ???
    def somethingElse():Int = ???
    def another(x:Int):String = ???  
  }

  def tests = TestSuite {
    
    /**
     * To a substantial degree, this test is testing the test infrastructure itself.
     */
    "Test url and ajax calls" - {
      setup() 
      registerApiHandler[ClientTestTrait]("getMsg")(new ClientTestEmpty with AutowireHandler {
        override def getMsg(partial:String) = s"ClientTestImpl got the message $partial"
    
        def handle(request:Core.Request[String]):Future[String] = route[ClientTestTrait](this)(request)
      })
      val controllers = interface[querki.comm.ApiComm].controllers
      val entryPoint:PlayCall = controllers.ClientController.apiRequest("User", "Space")
      val url = entryPoint.url
      assert(url == "/test/User/Space/apiRequest")
      assert(entryPoint.absoluteURL == "http://www.querki.net/test/User/Space/apiRequest")
          
      async {
        val result = await(Client[ClientTestTrait].getMsg("hi there").call())
	      assert(result == "ClientTestImpl got the message hi there")
      }
    }
    
  }
}
