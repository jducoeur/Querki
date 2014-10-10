package querki.display.input

import utest._
import scalatags.JsDom.all._

import querki.globals._

import querki.comm._
import querki.test._

object TagSetInputTests extends ThingPageTests {
  def tests = TestSuite {
    
    "Test menu functions" - {
      setupEcology()
      val controllers = interface[querki.comm.ApiComm].controllers
      val entryPoint:PlayCall = controllers.Application.thing("User", "Space", "Thing")
      println(s"entryPoint is $entryPoint")
      val url = entryPoint.url
      println(s"url is $url")
      println(s"TOS is ${controllers.TOSController.showTOS().url}")
    }
    
    "Prompting should work" - {
      setup(
        div()
      )
    }    
    
  }
}
