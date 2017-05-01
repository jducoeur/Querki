package querki.api

import models.{Kind, Wikitext}

import querki.data.{ThingInfo, TID}

/**
 * The "common" names used in Querki.
 * 
 * This is a bit of a hack, for letting the client and server share info about the major
 * API objects. These objects are listed in StandardThings, which connects the two sides
 * by Link Name. In order to not duplicate strings, the actual server objects use this
 * structure to fetch the official names from here.
 * 
 * The result is a tad clumsy, but strongly-typed and not duplicative. For example, the name of the
 * QList Collection is:
 * 
 * import querki.api.commonName
 * ...
 * commonName(_.core.listColl)
 * 
 * @author jducoeur
 */
object Names extends StandardThings(NamePassthroughHandler)

/**
 * This is a completely fake version of PassthroughHandler, which exists solely for the
 * purpose of storing the names of the fields in Names, so that they can be used in
 * the rest of the code. 
 */
private [api] object NamePassthroughHandler extends PassthroughHandlerBase {
  val tid = TID("")
  
  def pass(name:String):ThingInfo = {
    // The only field that matters here is the linkName:
    ThingInfo(
      tid,
      Some(name), 
      Wikitext(""),
      tid,
      Kind.Thing,
      false,
      false,
      false,
      false,
      false,
      None,
      Set.empty,
      Set.empty)
  }
}
