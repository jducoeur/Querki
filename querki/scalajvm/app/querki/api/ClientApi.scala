package querki.api

import upickle._

import querki.global._

import querki.data.{IdentityInfo, ThingInfo, UserInfo}
import querki.values.RequestContext

class ClientApiEcot(e:Ecology) extends QuerkiEcot(e) with ClientApi {
  
  def pickleThing(topt:Option[Thing]):String = {
    val info = topt.map(t => ThingInfo(t.id.toString, t.linkName, t.unsafeDisplayName))
    write(info)
  }
  
  def pickleMe(t:RequestContext):String = {
    val info = t.requester.map { user =>
      // TODO: this will need adjusting when we have multiple Identities. The mainIdentity should come first:
      val identityInfos = user.identities.map { identity =>
        IdentityInfo(identity.id.toString, identity.name, identity.handle)
      }
      UserInfo(user.id.toString, identityInfos)
    }
    write(info)
  }
}
