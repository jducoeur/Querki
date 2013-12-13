package controllers

import language.postfixOps
import QuerkiTemplate._

import querki.util._

/**
 * This implements a pub/sub interface similar to Publisher. However, it adds the concept
 * of "Contributor" -- subscribers don't just passively listen for events, they provide
 * contributions, which is kind of the point.
 */
object PageEventManager {
  val addHeaders = new PageAggregator
  val requestReceived = new RequestUpdater
}

/**
 * An event saying that a particular kind of Page is about to be displayed.
 */
case class HtmlEvent(rc:PlayRequestContext, template:QuerkiTemplate)

/**
 * A Publisher of page-display events.
 */
class PageAggregator extends Aggregator[HtmlEvent,String] {
  import play.api.templates.Html
  def apply(rc:PlayRequestContext, template:QuerkiTemplate):Html = {
    Html(collect(HtmlEvent(rc, template)) mkString("\n"))
  }
}

/**
 * A Publisher of request-received events.
 * 
 * This allows Modules to annotate the request, and is how we can decouple Modules properly.
 * However, note that there are some fairly serious inversion-of-control issues here, so use
 * this with caution, lest horribly hard-to-debug problems enter in!
 */
class RequestUpdater extends Sequencer[PlayRequestContext] {
  def apply(rc:PlayRequestContext):PlayRequestContext = {
    update(rc)
  }
}