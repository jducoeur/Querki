package querki.display.input

import utest._
import utest.ExecutionContext.RunNow

import scalatags.JsDom.all._

import querki.globals._

import querki.api._
import querki.test._

object TextInputTests extends InputTestBase {
  val propPath = "v-textoid-MyThingId"
        
  def tests = TestSuite {
    "A small Text should be editable" - {
        
      val newValue = "New text value"
        
      setupPage(
        // Guts of the Thing Page:
        div(
          input(cls:="_textEdit propEditor",
            tpe:="text",
            data("prop"):="textoid",
            name:=propPath
          )
        )
      )
      expectedChange { change =>
        assertMatch(change) { case EditFunctions.ChangePropertyValue(List(newValue)) => }
      }
      
      // Since things run synchronously, the page content should have filled by now:
      val elem = $("._textEdit")
      val fut = prepToChange(elem)
      
      // Fire the change:
      elem.text(newValue)
      elem.change()
      
      // Wait to be told that we're gotten to savecomplete:
      fut      
    }
    
    "A large Text should be editable" - {
        
      val newValue = """This is some new multi-paragraph text.
        |
        |It intentionally goes on for a while, to be vaguely realistic.
        |
        |It also includes *QText*, although that shouldn't really matter.""".stripMargin
        
      setupPage(
        // Guts of the Thing Page:
        div(
          input(cls:="_largeTextEdit propEditor",
            tpe:="text",
            data("prop"):="textoid",
            name:=propPath
          )
        )
      )
      expectedChange { change =>
        assertMatch(change) { case EditFunctions.ChangePropertyValue(List(newValue)) => }
      }
      
      // Since things run synchronously, the page content should have filled by now:
      val elem = $("._largeTextEdit")
      val fut = prepToChange(elem)
      
      // Fire the change:
      elem.text(newValue)
      elem.change()
      
      // Wait to be told that we're gotten to savecomplete:
      fut      
    }
  }
}
