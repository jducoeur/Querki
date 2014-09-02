package querki.client

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.JSApp

import org.scalajs.jquery.jQuery

import querki.shared.Test

object Hello extends JSApp {
  def main(): Unit = {
    jQuery(setupUI _)
  }
  
  def setupUI(): Unit = {
    jQuery("""<button type="button">Click me!</button>""")
      .click(addClickedMessage _)
      .appendTo(jQuery("body"))
	  
    appendPar(Test.hello)
  }
  
  def appendPar(text: String): Unit = {
    jQuery("body").append(s"<h3>$text</h3>")
  }

  @JSExport
  def addClickedMessage(): Unit = {
    appendPar("You clicked the button again!")
  }
}
