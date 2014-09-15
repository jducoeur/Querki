package querki.client

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.JSApp

import org.scalajs.jquery.{jQuery, JQueryEventObject}

import querki.shared.Test

import querki.ecology.test._

object Hello extends JSApp {
  def main(): Unit = {
    setupEcology()
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
  
  
  var theEcology:EcologyImpl = null
  
  def setupEcology() = {
    theEcology = new EcologyImpl
    new TestEcot(theEcology)
    theEcology.init
  }
}

trait TestInterface1 extends EcologyInterface {
  
}

trait TestInterface2 extends EcologyInterface {
  
}

class TestEcot(val ecology:Ecology) extends Ecot with TestInterface1 with TestInterface2 {
  
}
