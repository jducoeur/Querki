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
  val requestReceived = new RequestUpdater
}

/**
 * A listener that is receiving events and returning a result.
 */
trait Contributor[Evt,Result] {
  def notify(evt:Evt, sender:Publisher[Evt,Result]):Result
}

/**
 * The generic concept of a publisher for a particular kind of event.
 */
trait Publisher[Evt,Result] {
  import collection.mutable.Set
  type Cont = Contributor[Evt,Result]
  protected val contributors = Set.empty[Cont]
  
  def subscribe(cont:Cont) = contributors += cont
  def unsubscribe(cont:Cont) = contributors -= cont  
  
  // Synonyms for subscribe and unsubscribe, to satisfy my C# habits:
  def +=(cont:Cont) = subscribe(cont)
  def -=(cont:Cont) = unsubscribe(cont)
}

/**
 * A Publisher that collects the results of all of the Contributors into a Seq.
 */
trait Aggregator[Evt,Result] extends Publisher[Evt,Result] {
  def collect(evt:Evt):Seq[Result] = {
    contributors map (_.notify(evt, this)) toSeq
  }
}

/**
 * A Publisher that runs through all of the Contributors, and lets each one
 * update the event. So this effectively provides a way for listeners to mutate
 * the input.
 * 
 * IMPORTANT: order of processing is *not* yet guaranteed here! We might well make
 * some guarantees later (especially so that Modules can have a predictable order
 * based on their init dependencies), but don't count on that yet!
 */
trait Sequencer[Evt] extends Publisher[Evt, Evt] {
  def update(evt:Evt):Evt = {
    (evt /: contributors) ((current, contributor) => contributor.notify(current, this))
  }
}

/**
 * An event saying that a particular kind of Page is about to be displayed.
 */
case class HtmlEvent(rc:RequestContext, template:QuerkiTemplate)

/**
 * A Publisher of page-display events.
 */
class PageAggregator extends Aggregator[HtmlEvent,String] {
  import play.api.templates.Html
  def apply(rc:RequestContext, template:QuerkiTemplate):Html = {
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
class RequestUpdater extends Sequencer[RequestContext] {
  def apply(rc:RequestContext):RequestContext = {
    update(rc)
  }
}