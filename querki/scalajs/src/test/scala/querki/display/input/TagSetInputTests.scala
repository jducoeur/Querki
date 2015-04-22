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
      ) flatMap { page =>
        testChange(
          $("._tagSetInput"),
          propPath,
          mkChange = { elem =>
            elem.manifestAdd(Map(
	          "display" -> "My Second Tag",
	          "id" -> "My Second Tag"
	        ))
          },
          expected = stdChangePropMsg(propPath, Seq("My First Tag", "My Second Tag"))
        )
      }
    }    
    
  }
}
