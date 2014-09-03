package querki.client

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.JSApp

import org.scalajs.jquery.{jQuery, JQueryEventObject}

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
  def addClickedMessage(evt:JQueryEventObject): Unit = {
    val yet = "yet "
    val again = s"$yet again"
    appendPar(s"You clicked the button $again!")
  }
  
  @JSExport
  def fetchAMessage(basis:String):String = {
    s"$basis yourself!"
  }
}
