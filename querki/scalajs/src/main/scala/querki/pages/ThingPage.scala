package querki.pages

import upickle._

import org.scalajs.dom

import scalatags.JsDom.all._

import querki.globals._

class ThingPage(val ecology:Ecology, pickled:String) extends Page with EcologyMember {

  val info = read[ThingPageInfo](pickled)
  
  def title = info.thing.displayName
  
  def pageContent = raw(info.rendered)
}
