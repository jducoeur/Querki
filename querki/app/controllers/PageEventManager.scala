package controllers

import language.postfixOps
import QuerkiTemplate._

/**
 * This implements a pub/sub interface similar to Publisher. However, it adds the concept
 * of "Contributor" -- subscribers don't just passively listen for events, they provide
 * contributions, which is kind of the point.
 */
object PageEventManager {
  val addHeaders = new PageAggregator
}

trait Contributor[Evt,Result] {
  def notify(evt:Evt, sender:Aggregator[Evt,Result]):Result
}

trait Aggregator[Evt,Result] {
  import collection.mutable.Set
  type Cont = Contributor[Evt,Result]
  private val contributors = Set.empty[Cont]
  
  def subscribe(cont:Cont) = contributors += cont
  def unsubscribe(cont:Cont) = contributors -= cont
  
  def collect(evt:Evt):Seq[Result] = {
    contributors map (_.notify(evt, this)) toSeq
  }
}

case class HtmlEvent(rc:RequestContext, template:QuerkiTemplate)

class PageAggregator extends Aggregator[HtmlEvent,String] {
  import play.api.templates.Html
  def apply(rc:RequestContext, template:QuerkiTemplate):Html = {
    Html(collect(HtmlEvent(rc, template)) mkString("\n"))
  }
}