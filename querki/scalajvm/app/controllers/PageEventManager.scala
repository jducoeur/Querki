package controllers

import language.postfixOps

import play.twirl.api.Html

import QuerkiTemplate._

import querki.ecology._
import querki.util._

////////////////////////////////
//
// PUBLIC API
//

object PageMOIDs extends EcotIds(31)

/**
 * An event saying that a particular kind of Page is about to be displayed.
 */
case class HtmlEvent(
  rc: PlayRequestContext,
  template: QuerkiTemplate
)

trait PageAggregation extends Aggregator[HtmlEvent, String] {

  def apply(
    rc: PlayRequestContext,
    template: QuerkiTemplate
  ): Html
}

/**
 * This implements a pub/sub interface similar to Publisher. However, it adds the concept
 * of "Contributor" -- subscribers don't just passively listen for events, they provide
 * contributions, which is kind of the point.
 */
trait PageEventManager extends EcologyInterface {
  def addHeaders: PageAggregation
  def requestReceived: Sequencer[PlayRequestContext]
}

/////////////////////////////////
//
// IMPLEMENTATION
//

class PageEventManagerEcot(e: Ecology) extends QuerkiEcot(e) with PageEventManager {
  lazy val addHeaders = new PageAggregator
  lazy val requestReceived = new RequestUpdater
}

/**
 * A Publisher of page-display events.
 */
class PageAggregator extends PageAggregation {

  def apply(
    rc: PlayRequestContext,
    template: QuerkiTemplate
  ): Html = {
    Html(collect(HtmlEvent(rc, template)).mkString("\n"))
  }
}

/**
 * A Publisher of request-received events.
 *
 * This allows Modules to annotate the request, and is how we can decouple Modules properly.
 * However, note that there are some fairly serious inversion-of-control issues here, so use
 * this with caution, lest horribly hard-to-debug problems enter in!
 */
class RequestUpdater extends Sequencer[PlayRequestContext]
