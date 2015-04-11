package querki.display.input

import scala.scalajs.js
import js.JSConverters._

import utest._
import scalatags.JsDom.all._
import org.querki.jquery._

import querki.globals._

import querki.api._
import querki.test._

import org.querki.facades.manifest._

object TagSetInputTests extends InputTestBase {
        
  val propPath = "v-linksetpropoid-MyThingId"
    
  def tests = TestSuite {
    
    // Note that we can't easily test the Manifest functions themselves yet, since we don't have
    // an obvious way to load the Manifest Javascript code into the test harness. But we can at
    // least check the gadget itself a bit:
    "The TagSetInput Gadget should hook in, and respond to manifestchanges" - {
      setupPage(
        // Guts of the Thing Page:
        div(
          input(cls:="_tagSetInput propEditor",
            tpe:="text",
            data("isnames"):=true,
            data("current"):="""[{"display":"My First Tag", "id":"My First Tag"}]""",
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
        assertMatch(change) { case EditFunctions.ChangePropertyValue(propPath, List("My First Tag", "My Second Tag")) => }
      }
      
      // Since things run synchronously, the page content should have filled by now:
      val manifestBase = $("._tagSetInput")
      val fut = prepToChange(manifestBase)
      
      // Change the contents of the Manifest, which should cause the save message to go out:
      manifestBase.manifestAdd(Map(
        "display" -> "My Second Tag",
        "id" -> "My Second Tag"
      ))
      
      // Wait to be told that we're gotten to savecomplete:
      fut
    }    
    
  }
}
