package querki.display

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.jquery.JQueryEventObject
import scalatags.JsDom.all._
import autowire._

import querki.globals._

import querki.api.ThingFunctions

class QLButtonGadget[Output <: dom.Element](tag:scalatags.JsDom.TypedTag[Output])(implicit val ecology:Ecology) extends Gadget[Output] with EcologyMember {
  
  lazy val Client = interface[querki.client.Client]
  
  def doRender() = tag
  
  override def onCreate(elem:Output) = {
    val jq = $(elem)
    val thingId = jq.data("thingid").asInstanceOf[String]
    val ql = jq.data("ql").asInstanceOf[String]
    val target = jq.data("target").asInstanceOf[String]
    
    jq.click { (evt:JQueryEventObject) =>
      Client[ThingFunctions].evaluateQL(thingId, ql).call().foreach { result =>
        val qtext = new QText(result)
        val targetJQ = $(s"#$target")
        targetJQ.empty()
        targetJQ.append(qtext.render)
        targetJQ.show()
      }
    }
  }
}
