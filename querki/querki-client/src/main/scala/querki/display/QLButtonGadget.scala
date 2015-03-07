package querki.display

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery.JQueryEventObject
import scalatags.JsDom.all._
import autowire._

import querki.globals._

import querki.api.ThingFunctions
import querki.display.input.InputGadget

class QLButtonGadget[Output <: dom.Element](tag:scalatags.JsDom.TypedTag[Output])(implicit e:Ecology) extends InputGadget[Output](e) with EcologyMember {
  
  lazy val InputGadgets = interface[querki.display.input.InputGadgets]
  
  def doRender() = tag
  def values = ???
  
  def hook() = {
    val jq = $(elem)
    val thingId = jq.tidString("thingid")
    val ql = jq.data("ql").asInstanceOf[String]
    val target = jq.data("target").asInstanceOf[String]
    
    jq.click { (evt:JQueryEventObject) =>
      Client[ThingFunctions].evaluateQL(thingId, ql).call().foreach { result =>
        val qtext = new QText(result)
        val targetJQ = $(s"#$target")
        targetJQ.empty()
        targetJQ.append(qtext.render)
        targetJQ.show()
        InputGadgets.hookPendingGadgets()
      }
      evt.preventDefault()
    }
  }
}
