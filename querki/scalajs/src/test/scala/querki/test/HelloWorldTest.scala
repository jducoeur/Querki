package querki.test

import scala.scalajs.js

import utest._

import org.scalajs.dom
import org.querki.jquery._
import scalatags.JsDom.all._

import querki.globals._

object HelloWorldTest extends TestSuite {
  def tests = TestSuite {
    'Hello {
      
      val h = head(scalatags.JsDom.tags2.title("This is the title")).render
      $(h).appendTo($("body"))
      
//      println("The title is now " + $("title").text)
      
      assert(1 == 1)
      
      // I can update a literal easily, because it is js.Dynamic:
      val d:js.Dynamic = lit()
      d.hello = ((t:String) => s"hello $t")
//      println(s"Dynamic hello is ${d.hello(" there")}")
    }
  }
}
