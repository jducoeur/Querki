package controllers

import models.OID

import querki.ecology._
import querki.html.PublicUrls
import querki.values.RequestContext

/**
 * This is the glue that exposes the relevant parts of the reverse router without causing
 * the rest of the system to be directly dependent on the routing table. There should *never*
 * be direct references from non-Play code to the reverse router!
 * 
 * Note that the PublicUrls interface is defined in querki.html, so that the rest of the code
 * doesn't have to depend on controllers.
 */
class PublicUrlDefinitions(e:Ecology) extends QuerkiEcot(e) with PublicUrls {
  def createAndEditUrl(rc:RequestContext, modelId:OID):String = {
    rc match {
      case prc:PlayRequestContext => {
        implicit val req = prc.request
        val call = routes.Application.doCreateThing2(rc.ownerHandle, rc.state.get.toThingId, modelId.toThingId)
        call.absoluteURL()
      }
      case _ => throw new Exception("PublicUrlDefinitions somehow got a non-Play RequestContext!")
    }
  }
}