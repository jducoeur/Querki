package querki.apps

import querki.api.{AutowireParams, SpaceApiImpl}
import querki.data.SpaceInfo
import querki.globals._

/**
 * @author jducoeur
 */
class AppsFunctionsImpl(info:AutowireParams)(implicit e:Ecology) extends SpaceApiImpl(info, e) with AppsFunctions  {
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  
  def doRoute(req:Request):Future[String] = route[AppsFunctions](this)(req)
  
  def getApps():Seq[SpaceInfo] = {
    for {
      app <- state.apps
    }
      yield ClientApi.spaceInfo(app)
  }
}
