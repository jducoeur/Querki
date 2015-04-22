package querki.display.input

import utest._

import scalatags.JsDom.all._

import org.querki.jquery._

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
      ) flatMap { page =>
        testChange(
          $("._textEdit"),
          propPath,
          mkChange = { elem =>
	        elem.value(newValue)
	        elem.change()
          },
          expected = stdChangePropMsg(propPath, Seq(newValue))
        )
      }
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
      ) flatMap { page =>
        testChange(
          $("._largeTextEdit"),
          propPath,
          mkChange = { elem =>
  	        elem.value(newValue)
	        elem.change()
          },
          // It turns out that jQuery does *not* entirely preserve the value of a textarea!!! Why is this working properly
          // in the real code???
          expected = stdChangePropMsg(propPath, Seq(newValue.replaceAll("\\r\\n", " ")))
        )
      }  
    }
  }
}
