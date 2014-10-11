package querki.display.input

import utest._
import scalatags.JsDom.all._

import querki.globals._

import querki.comm._
import querki.test._
import querki.util.ScalatagUtils

object TagSetInputTests extends ThingPageTests with ScalatagUtils {
  def tests = TestSuite {
    
    "Test stuff" - {
      setupEcology()
      val controllers = interface[querki.comm.ApiComm].controllers
      val entryPoint:PlayCall = controllers.Application.thing("User", "Space", "Thing")
      println(s"entryPoint is $entryPoint")
      val url = entryPoint.url
      println(s"url is $url")
      println(s"TOS is ${controllers.TOSController.showTOS().url}")
      
      val request = controllers.ClientController.apiRequest("User", "Space", "Pickled")
      println(s"AJAX call is $request")
      val ajaxRequest:PlayAjax = request
      println(s"AJAX result is ${ajaxRequest.callAjax()}")
    }
    
    // Note that we can't easily test the Manifest functions themselves yet, since we don't have
    // an obvious way to load the Manifest Javascript code into the test harness. But we can at
    // least check the gadget itself a bit:
    "The TagSetInput Gadget should hook in, and respond to manifestchanges" - {
      setup(
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
