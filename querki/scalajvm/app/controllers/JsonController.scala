package controllers

import javax.inject._

import upickle._
import autowire._

import models._

import querki.api._
import querki.data._
import querki.globals._
import querki.imexport.JsonFunctions

class JsonController @Inject() (val appProv: Provider[play.api.Application]) extends ApplicationBase {

  def json(
    ownerId: String,
    spaceIdStr: String,
    thingIdStr: String,
    propIdStr: String
  ) = withLocalClient(ownerId, spaceIdStr) { (rc, client) =>
    implicit val r = rc
    client[ThingFunctions].getRequestInfo().call().flatMap { requestInfo =>
      if (requestInfo.forbidden) {
        unknownSpace(spaceIdStr)
      } else {
        val thingId = ThingId(thingIdStr)
        val tid = TID(thingId.toString)
        val propId =
          if (propIdStr.isEmpty)
            None
          else
            Some(TID(ThingId(propIdStr).toString))
        for {
          json <- client[JsonFunctions].getJsonFor(tid, propId).call()
        } yield Ok(json)
      }
    }.recoverWith {
      case pex: PublicException => doError(indexRoute, pex)
    }
  }
}
