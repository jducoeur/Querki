package controllers

import javax.inject._

import upickle._
import autowire._

import models._

import play.api.mvc.Call

import querki.api._
import querki.data._
import querki.globals._

/**
 * The Controller for the "raw" version of pages. This is what crawlers get automatically sent to
 * (by detecting their user agents), and is also available for, eg, screen readers.
 * 
 * @author jducoeur
 */
class RawController @Inject() (val appProv:Provider[play.api.Application]) extends ApplicationBase {
  def thing(ownerId:String, spaceIdStr:String, thingIdStr:String) = withLocalClient(ownerId, spaceIdStr) { (rc, client) =>
    implicit val r = rc
    client[ThingFunctions].getRequestInfo().call().flatMap { requestInfo =>
      if (requestInfo.forbidden) {
        unknownSpace(spaceIdStr)
      } else {
        val thingId = ThingId(thingIdStr)
        val tid = TID(thingId.toString())
        for {
          thingPageDetails <- client[ThingFunctions].getThingPage(tid, None).call()
          descOpt <- client[ThingFunctions].getPropertyDisplay(tid, querki.conventions.MOIDs.PropDetailsOID.toTID).call()
          title = thingPageDetails.thingInfo.displayName
          canonical = new Call(rc.request.method, rc.request.uri).absoluteURL(true)(rc.request)
          descWiki = descOpt.getOrElse(thingPageDetails.rendered)
          desc = descWiki.displayWith(new LiteralTransformWrapper).toString
          guts = thingPageDetails.rendered.display.toString
        }
          yield Ok(views.html.raw(title, canonical, desc, guts, rc.request))
      }
    } recoverWith {
      case pex:PublicException => doError(indexRoute, pex) 
    }    
  }
}
