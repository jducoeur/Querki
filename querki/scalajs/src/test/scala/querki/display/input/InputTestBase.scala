package querki.display.input

import scala.concurrent.{Future, Promise}

import org.scalajs.jquery.JQuery

import utest._
import utest.ExecutionContext.RunNow
import autowire._

import querki.globals._

import querki.api.{EditFunctions, EditFunctionsEmpty}
import EditFunctions._
import querki.test._
import querki.util.ScalatagUtils

trait InputTestBase extends ThingPageTests with ScalatagUtils {
  def propPath:String
  
  def expectedChange(test:PropertyChange => Unit) = {
    registerApiHandler[EditFunctions]("alterProperty")(new EditFunctionsEmpty with AutowireHandler {
      override def alterProperty(thingId:String, path:String, change:PropertyChange):PropertyChangeResponse = {
        test(change)
        assert(path == propPath)
        PropertyChanged
      }
    
      def handle(request:Core.Request[String]):Future[String] = route[EditFunctions](this)(request)
    })    
  }
  
  def prepToChange(elem:JQuery):Future[Unit] = {
    assert(elem.length == 1)
    val promise = Promise[String]
    // savecomplete is triggered when InputGadget receives a PropertyChanged from the server.
    // We need to set it now, because things happen synchronously in utest:
    elem.on("savecomplete", { () => promise.success("Got it") })
    // Wait to be told that we're gotten to savecomplete:
    promise.future.map { result =>
      assert(result == "Got it")
      assert($("#statusText").text == "Saved")
    }    
  }
}