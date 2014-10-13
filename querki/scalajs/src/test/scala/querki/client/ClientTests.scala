package querki.client

import scala.concurrent.Future

import scala.async.Async._

import utest._
import autowire._
import utest.ExecutionContext.RunNow

import querki.comm._
import querki.test._

object ClientTests extends QuerkiTests with autowire.Server[String, upickle.Reader, upickle.Writer] {
  
  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
  
  lazy val Client = interface[querki.client.Client]
  
  trait ClientTestTrait {
    def getMsg(partial:String):String
  }
  
  object ClientTestImpl extends ClientTestTrait {
    def getMsg(partial:String) = s"ClientTestImpl got the message $partial"
  }
  
  override def apiHandler(request:Core.Request[String]):Future[String] = {
    route[ClientTestTrait](ClientTestImpl)(request)
  }

  def tests = TestSuite {
    
    "Test url and ajax calls" - {
      setup()
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