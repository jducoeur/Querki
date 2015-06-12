package controllers

import play.api.mvc.Call

import models.ThingId

import querki.ecology._
import querki.html.PublicUrls
import querki.values.{RequestContext, SpaceState}

/**
 * This is the glue that exposes the relevant parts of the reverse router without causing
 * the rest of the system to be directly dependent on the routing table. There should *never*
 * be direct references from non-Play code to the reverse router!
 * 
 * Note that the PublicUrls interface is defined in querki.html, so that the rest of the code
 * doesn't have to depend on controllers.
 */
class PublicUrlDefinitions(e:Ecology) extends QuerkiEcot(e) with PublicUrls {
  def createAndEditUrl(rc:RequestContext, modelId:ThingId)(implicit state:SpaceState):String = {
    // TODO: this code arguably belongs in ClientController somehow, but I'd prefer to not
    // force a pointless redirect:
    val spaceCall = routes.ClientController.space(rc.ownerHandle, state.toThingId)
    val call = new Call(spaceCall.method, spaceCall.url + s"#_createAndEdit?model=$modelId")
    call.url
  }
}