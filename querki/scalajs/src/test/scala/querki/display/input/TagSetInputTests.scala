package querki.display.input

import scala.scalajs.js
import js.JSConverters._

import utest._
import scalatags.JsDom.all._
import org.scalajs.jquery._

import querki.globals._

import querki.api._
import querki.test._

object TagSetInputTests extends InputTestBase {
        
  val propPath = "v-linksetpropoid-MyThingId"
    
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
      
      setupPage(
        // Guts of the Thing Page:
        div(
          input(cls:="_tagSetInput propEditor",
            tpe:="text",
            data("isnames"):=false,
            data("current"):="""[{"display":"First Thing", "id":".firstthing"}, {"display":"Second Thing", "id":"secondthing"}]""",
            data("prop"):="linksetpropoid",
            name:=propPath
          )
        ),
        // Stub for the marcoPolo entry point, so we can have a URL:
        Some({controllers => controllers.ClientController.marcoPolo = { entryPoint1("marcoPolo") _ }})
      )
      // This will be called when we trigger the manifestchange -- it is the stub for the server call to save the
      // value.
      expectedChange { change =>
        assertMatch(change) { case EditFunctions.ChangePropertyValue(List("My First Tag", "My Second Tag")) => }
      }
      
      // Since things run synchronously, the page content should have filled by now:
      val manifestBase = $("._tagSetInput")
      val fut = prepToChange(manifestBase)
      
      // Tell the TagSetInput Gadget that the Manifest has changed:
      // TODO: once we figure out how to load Manifest's Javascript in tests, try to do this more
      // realistically:
      manifestBase.trigger("manifestchange", Seq("My First Tag", "My Second Tag").toJSArray)
      
      // Wait to be told that we're gotten to savecomplete:
      fut
    }    
    
  }
}
