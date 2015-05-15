package querki.test

import models.ThingId

import querki.globals._
import querki.html.PublicUrls
import querki.values.RequestContext

class PublicUrlStub(e:Ecology) extends QuerkiEcot(e) with PublicUrls {
  def createAndEditUrl(rc:RequestContext, modelId:ThingId):String = {
    s"http://querki/#_createAndEdit?model=$modelId"
  }
}
