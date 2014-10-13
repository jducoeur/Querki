package querki.display.input

import utest._
import autowire._
import scalatags.JsDom.all._

import querki.globals._

import querki.comm._
import querki.test._
import querki.util.ScalatagUtils

object TagSetInputTests extends ThingPageTests with ScalatagUtils {
  
  def tests = TestSuite {
    
    // Note that we can't easily test the Manifest functions themselves yet, since we don't have
    // an obvious way to load the Manifest Javascript code into the test harness. But we can at
    // least check the gadget itself a bit:
    "The TagSetInput Gadget should hook in, and respond to manifestchanges" - {
      setupPage(
        div(
          input(cls:="_tagSetInput propEditor",
            tpe:="text",
            data("isnames"):=false,
            data("current"):="""[{"display":"First Thing", "id":".firstthing"}, {"display":"Second Thing", "id":"secondthing"}]""",
            data("propid"):="linksetpropoid"
          )
        )
      )
    }    
    
  }
}
