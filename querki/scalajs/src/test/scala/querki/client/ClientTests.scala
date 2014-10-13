package querki.client

import scala.concurrent.Future

import scala.async.Async._

import utest._
import autowire._
import utest.ExecutionContext.RunNow

import querki.comm._
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
    
    "Test url and ajax calls" - {
      setup() 
      registerApiHandler[ClientTestTrait](new ClientTestEmpty with AutowireHandler {
        override def getMsg(partial:String) = s"ClientTestImpl got the message $partial"
    
        def handle(request:Core.Request[String]):Future[String] = route[ClientTestTrait](this)(request)
      })
      val controllers = interface[querki.comm.ApiComm].controllers
      val entryPoint:PlayCall = controllers.Application.thing("User", "Space", "Thing")
      val url = entryPoint.url
      assert(url == "/test/User/Space/thing/Thing")
      
      async {
        val result = await(Client[ClientTestTrait].getMsg("hi there").call())
	    assert(result == "ClientTestImpl got the message hi there")
      }
    }
    
  }
  
}