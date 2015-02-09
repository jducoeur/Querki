package querki.security

import querki.globals._

import querki.api.SecurityFunctions
import querki.data._
import querki.session.{AutowireApiImpl, AutowireParams}

class SecurityFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends AutowireApiImpl(info, e) with SecurityFunctions {
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val Roles = interface[Roles]
  
  def getRoles():Seq[ThingInfo] = {
    val roles = Roles.allRoles(info.state)
    roles.map(ClientApi.thingInfo(_, rc))
  }
}
