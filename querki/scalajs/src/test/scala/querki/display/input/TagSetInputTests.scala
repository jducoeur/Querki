package querki.display.input

import scala.concurrent.{Future, Promise}
import scala.async.Async._

import scala.scalajs.js
import js.JSConverters._

import utest._
import utest.ExecutionContext.RunNow
import autowire._
import scalatags.JsDom.all._
import org.scalajs.jquery._

import querki.globals._

import querki.api._
import querki.comm._
import querki.test._
import querki.util.ScalatagUtils

object TagSetInputTests extends ThingPageTests with ScalatagUtils {
  
  def tests = TestSuite {
    
    // Note that we can't easily test the Manifest functions themselves yet, since we don't have
    // an obvious way to load the Manifest Javascript code into the test harness. But we can at
    // least check the gadget itself a bit:
    "The TagSetInput Gadget should hook in, and respond to manifestchanges" - {
      
      // Monkey-patch in a stub of the main Manifest call, so the TagSetInput gadget doesn't crash:
      $.fn.asInstanceOf[js.Dynamic].manifest = { (p:Any) =>
        p match {
          case "values" => Seq("My First Tag", "My Second Tag").toJSArray
          case _ => $("body")
        }
      }
      
      val propPath = "v-linksetpropoid-MyThingId"
      
      setupPage(
        div(
          input(cls:="_tagSetInput propEditor",
            tpe:="text",
            data("isnames"):=false,
            data("current"):="""[{"display":"First Thing", "id":".firstthing"}, {"display":"Second Thing", "id":"secondthing"}]""",
            data("prop"):="linksetpropoid",
            name:=propPath
          )
        ),
        Some({controllers => controllers.ClientController.marcoPolo = { entryPoint1("marcoPolo") _ }})
      )
      registerApiHandler[EditFunctions]("alterProperty")(new EditFunctionsEmpty with AutowireHandler {
        import EditFunctions._
        override def alterProperty(thingId:String, path:String, change:PropertyChange):PropertyChangeResponse = {
          assertMatch(change) { case ChangePropertyValue(List("My First Tag", "My Second Tag")) => }
          assert(path == propPath)
          PropertyChanged
        }
    
        def handle(request:Core.Request[String]):Future[String] = route[EditFunctions](this)(request)
      })
      
      // Since things run synchronously, the page content should have filled by now:
      val manifestBase = $("._tagSetInput")
      assert(manifestBase.length == 1)
      
      // Now -- can we trigger a change, and get the save message out?
      val promise = Promise[String]
      manifestBase.on("savecomplete", { () => promise.success("Got it") })
      manifestBase.trigger("manifestchange", Seq("My First Tag", "My Second Tag").toJSArray)
      promise.future.map { result =>
        assert(result == "Got it")
      }
    }    
    
  }
}
