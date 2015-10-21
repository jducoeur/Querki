package querki.apps

import models.{Property}

import querki.ecology._
import querki.globals._
import querki.spaces._

object MOIDs extends EcotIds(59) {
  val CanBeAppOID = moid(1)
}

private [apps] trait AppsInternal extends EcologyInterface {
  def CanUseAsAppProp:Property[OID, OID]
}

/**
 * @author jducoeur
 */
class AppsEcot(e:Ecology) extends QuerkiEcot(e) with SpacePluginProvider with AppsInternal {
  import MOIDs._
  
  val AccessControl = initRequires[querki.security.AccessControl]
  
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val SpaceChangeManager = interface[querki.spaces.SpaceChangeManager]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  
  override def postInit() = {
    // Some entry points are legal without login:
    ApiRegistry.registerApiImplFor[AppsFunctions, AppsFunctionsImpl](SpaceOps.spaceRegion, false)
  }
  
  /**
   * Called by each Space once, to instantiate its plugins. This is how we hook Space processing.
   */
  def createPlugin(space:SpaceAPI):SpacePlugin = new AppsSpacePlugin(space, ecology)
    
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val CanUseAsAppProp = AccessControl.definePermission(CanBeAppOID, 
      "Can Use as an App",
      "These people are allowed to use this Space as an App. **Use with caution! These people will be able to see everything in this Space!**",
      Seq(AccessControl.OwnerTag),
      true)
  
  override lazy val props = Seq(
    CanUseAsAppProp
  )
}
