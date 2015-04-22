package querki.display.input

import scala.concurrent.{Future, Promise}
import org.scalajs.dom
import org.querki.jquery._
import org.querki.jsext._

import utest._
import autowire._

import querki.globals._

import querki.api.{EditFunctions, EditFunctionsEmpty}
import EditFunctions._
import querki.test._
import querki.util.ScalatagUtils

trait InputTestBase extends ThingPageTests with ScalatagUtils {
  def propPath:String
  
  // DEPRECATED -- switch to the more-complete testChange()
  def expectedChange(test:PropertyChange => Unit) = {
    registerApiHandler[EditFunctions]("alterProperty")(new EditFunctionsEmpty with AutowireHandler {
      override def alterProperty(thingId:TID, change:PropertyChange):Future[PropertyChangeResponse] = {
        Future {
          test(change)
          assert(change.path == propPath)
          PropertyChanged
        }
      }
    
      def handle(request:Core.Request[String]):Future[String] = route[EditFunctions](this)(request)
    })    
  }
  
  // DEPRECATED -- switch to the more-complete testChange()
  def prepToChange(elem:JQuery):Future[Unit] = {
    assert(elem.length == 1)
    val promise = Promise[String]
    // savecomplete is triggered when InputGadget receives a PropertyChanged from the server.
    // We need to set it now, because things happen synchronously in utest:
    elem.on("savecomplete", { (e:dom.Element) => promise.success("Got it") })
    // Wait to be told that we're gotten to savecomplete:
    println("Have set the savecomplete callback")
    promise.future.map { result =>
      assert(result == "Got it")
      assert($("#statusText").text == "Saved")
      println("Got all the way through")
    }    
  }
  
  def testChange(elem:JQuery, mkChange:JQuery => Unit, expected:PartialFunction[PropertyChange, Unit]):Future[Unit] = {
    assert(elem.length == 1)
    val promise = Promise[Unit]
    
    // Register the handler that simulates the server side, which will test that we got the expected message:
    registerApiHandler[EditFunctions]("alterProperty")(new EditFunctionsEmpty with AutowireHandler {
      override def alterProperty(thingId:TID, change:PropertyChange):Future[PropertyChangeResponse] = {
        Future {
          val failFunc:PartialFunction[PropertyChange,Unit] = { 
            case _ => {
              val ex = new Exception(s"Got unexpected change result $change")
              promise.failure(ex)
              throw ex
            }
          }
          (expected orElse failFunc)(change)
          assert(change.path == propPath)
          PropertyChanged
        }
      }
    
      def handle(request:Core.Request[String]):Future[String] = route[EditFunctions](this)(request)
    })
    
    // savecomplete is triggered when InputGadget receives a PropertyChanged from the server. So the last step
    // will be to handle this event, which means that all is copacetic:
    elem.on("savecomplete", { (e:dom.Element) => 
      assert($("#statusText").text == "Saved")
      promise.success() 
    })
    
    // Finally, now that the handlers are set up, trigger the change itself:
    mkChange(elem)
    
    promise.future.withTimeout
  }
}
