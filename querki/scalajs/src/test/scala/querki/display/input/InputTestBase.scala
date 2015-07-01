package querki.display.input

import scala.concurrent.{Future, Promise}
import org.scalajs.dom
import org.querki.jquery._
import org.querki.jsext._
import utest._
import autowire._
import querki.globals._
import querki.editing.{EditFunctions, EditFunctionsEmpty}
import EditFunctions._
import querki.test._
import querki.util.ScalatagUtils

trait InputTestBase extends ThingPageTests with ScalatagUtils {

  /**
   * Concise wrapper to define the "expected" value for a normal ChangePropertyValue message.
   * 
   * @param propPath The expected Path for the Property to change.
   * @param newValue The expected stringified value.
   */
  def stdChangePropMsg(propPath:String, newValue:Seq[String]):PartialFunction[PropertyChange, Unit] = {
    { case EditFunctions.ChangePropertyValue(pp, nv) if (pp == propPath && nv == newValue) => {} }
  }
  
  /**
   * Test a change to an InputGadget.
   * 
   * @param elem The JQuery handle to the element we're going to change.
   * @param propPath The expected path to the Property being changed.
   * @param mkChange A block that actually makes the change and causes save() to be invoked in some natural way.
   *   This will often call jQuery.change() to fire the event.
   * @param expected A PartialFunction that matches the PropertyChange message we expected to receive on the server.
   * 
   * @returns A Future that will succeed if everything goes smoothly, or contain an Exception if not. Note that this
   *   will automatically time out if it fails.
   */
  def testChange(elem:JQuery, propPath:String, mkChange:JQuery => Unit, expected:PartialFunction[PropertyChange, Unit]):Future[Unit] = {
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
