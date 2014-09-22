package querki.api

import upickle._

import querki.global._

import querki.data.ThingInfo

class ClientApiEcot(e:Ecology) extends QuerkiEcot(e) with ClientApi {
  
  def pickleThing(topt:Option[Thing]):String = {
    val info = topt.map(t => ThingInfo(t.id.toString, t.linkName, t.unsafeDisplayName))
    write(info)
  }
}
