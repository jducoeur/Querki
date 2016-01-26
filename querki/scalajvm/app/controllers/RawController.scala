package controllers

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
class RawController extends ApplicationBase {
  def thing(ownerId:String, spaceIdStr:String, thingIdStr:String) = withLocalClient(ownerId, spaceIdStr) { (rc, client) =>
    implicit val r = rc
    client[ThingFunctions].getRequestInfo().call().flatMap { requestInfo =>
      if (requestInfo.forbidden) {
        unknownSpace(spaceIdStr)
      } else {
        val thingId = ThingId(thingIdStr)
        client[ThingFunctions].getThingPage(TID(thingId.toString()), None).call().map { thingPageDetails =>
          val title = thingPageDetails.thingInfo.displayName
          val canonical = new Call(rc.request.method, rc.request.uri).absoluteURL(false)(rc.request)
          val desc = thingPageDetails.rendered.plaintext
          val guts = thingPageDetails.rendered.display.toString
          Ok(views.html.raw(title, canonical, desc, guts, rc.request))
        }
      }
    } recoverWith {
      case pex:PublicException => doError(indexRoute, pex) 
    }    
  }
}