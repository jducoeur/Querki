/*
package querki.client

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.JSApp

import org.scalajs.jquery.{jQuery, JQueryEventObject}

import querki.ecology._
import querki.qtext.ActuariusTransformer
import querki.shared.Test

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
    
    setupLiveWikitext()
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
  
  def body = jQuery("body")
  
  lazy val transformer = new ActuariusTransformer
  
  def setupLiveWikitext():Unit = {
    val inputArea = jQuery("""<textarea rows="5" style="width: 100%"></textarea>""")
      .appendTo(body)
    val outputArea = jQuery("""<div style="width:100%; background: #dddddd"></div>""")
      .appendTo(body)
      
    inputArea.keyup { (evt:JQueryEventObject) =>
      val wikitext = inputArea.value.toString
      val html = transformer(wikitext)
      outputArea.html(html)
    }
  }
  
  var theEcology:EcologyImpl = null
  
  def setupEcology() = {
    theEcology = new EcologyImpl
    new TestEcot(theEcology)
    theEcology.init(ClientState()) { state => state }
  }
}

trait TestInterface1 extends EcologyInterface {
  
}

trait TestInterface2 extends EcologyInterface {
  
}

class TestEcot(e:Ecology) extends ClientEcot(e) with TestInterface1 with TestInterface2 {
  def implements = Set(classOf[TestInterface1], classOf[TestInterface2])
}
*/
