package querki.test

import scala.scalajs.js

import utest._

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._

import querki.globals._

object HelloWorldTest extends TestSuite {
  def tests = TestSuite {
    'Hello {
      
      val h = head(scalatags.JsDom.tags2.title("This is the title")).render
      $(h).appendTo($("body"))
      
      println("The title is now " + $("title").text)
      
      assert(1 == 1)
      
      // I can update a literal easily, because it is js.Dynamic:
      val d:js.Dynamic = lit()
      d.hello = ((t:String) => s"hello $t")
      println(s"Dynamic hello is ${d.hello(" there")}")
//      
//      // But that doesn't let me do so with a string name. For that I need an Object:
//      val o:js.Object with js.Dynamic = lit()
//      js.Object.defineProperty(o, "hi", lit(there = "there").asInstanceOf[js.PropertyDescriptor])
//      println(s"Object hello is ${o.hi.there}")
    }
  }
}
