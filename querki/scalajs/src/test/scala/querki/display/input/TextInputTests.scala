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
          expected = { case EditFunctions.ChangePropertyValue(pp, Vector(nv)) if (pp == propPath && nv == newValue) => {} }
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
  	        val storedValue = elem.valueString
  	        println(s"Original value was $newValue")
  	        println(s"After jQuery, value is $storedValue")
  	        println(s"Stripped original is ${newValue.replaceAll("\\r\\n", " ")}")
	        elem.change()
          },
          expected = { case EditFunctions.ChangePropertyValue(pp, Vector(nv)) if (pp == propPath && nv == newValue.replaceAll("\\r\\n", " ")) => {} }
        )
      }  
    }
  }
}
