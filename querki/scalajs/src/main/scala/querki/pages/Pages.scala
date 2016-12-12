package querki.pages

import querki.globals._

import querki.comm.URL
import querki.data.SpaceInfo
import querki.search.SearchResultsPage

import querki.display.ManagedFrag

class PagesEcot(e:Ecology) extends ClientEcot(e) with Pages {
  
  def implements = Set(classOf[Pages])
  
  lazy val DataAccess = interface[querki.data.DataAccess]
  lazy val PageManager = interface[querki.display.PageManager]

  // Factories for some pages with no obvious homes:
  lazy val exploreFactory = registerThingPageFactory("_explore", { (params) => new ExplorePage(params) }, "thingId")
  lazy val viewFactory = registerThingPageFactory("_view", { (params) => new ViewPage(params) }, "thingId")
  lazy val createAndEditFactory = registerThingPageFactory("_createAndEdit", { (params) => new CreateAndEditPage(params) }, "model")
  lazy val sharingFactory = registerStandardFactory("_sharing", { (params) => new SharingPage })
  lazy val advancedFactory = registerThingPageFactory("_advanced", { (params) => new AdvancedPage(params) }, "thingId")
  lazy val indexFactory = registerStandardFactory("_index", { (params) => new IndexPage(params) })
  lazy val accountFactory = registerStandardFactory("_account", { (params) => new AccountPage(params) })
  lazy val createSpaceFactory = registerStandardFactory("_createSpace", { (params) => new CreateSpacePage(params) })
  lazy val importSpaceFactory = registerStandardFactory("_importSpace", { (params) => new ImportSpacePage(params) })
  lazy val securityFactory = registerThingPageFactory("_security", { (params) => new querki.security.SecurityPage(params) }, "thingId")
  lazy val signupFactory = registerStandardFactory("_signup", { (params) => new querki.identity.SignUpPage })
  lazy val validateSignupFactory = registerStandardFactory("_validateSignup", { (params) => new querki.identity.ValidateSignupPage(params) })
  lazy val infoFactory = registerStandardFactory("_spaceInfo", { (params) => new InfoPage(params) })
  
  lazy val thingPageFactory = new RawThingPageFactory
  
  override def postInit() = {
    exploreFactory
    viewFactory
    createAndEditFactory
    sharingFactory
    advancedFactory
    indexFactory
    accountFactory
    createSpaceFactory
    importSpaceFactory
    thingPageFactory
    securityFactory
    signupFactory
    validateSignupFactory
    infoFactory
  }
  
  private var factories = Seq.empty[PageFactory]
  
  def registerFactory[T <: PageFactory](factory:T):T = {
    factories :+= factory
    factory
  }
  
  def registerStandardFactory(registeredName:String, const:ParamMap => Page):PageFactory = {
    registerFactory(new PageFactoryBase(registeredName, const))    
  }
  
  def registerThingPageFactory(registeredName:String, const:ParamMap => Page, paramName:String):ThingPageFactory = {
    registerFactory(new ThingPageFactoryBase(registeredName, const, paramName))    
  }  
  
  // TODO: this doesn't yet work correctly to navigate cross-Spaces:
  def showSpacePage(space:SpaceInfo) = thingPageFactory.showPage(space)  
  def spaceUrl(space:SpaceInfo):URL = thingPageFactory.pageUrl(space)
  
  /**
   * Construct the correct Page, based on the passed-in page name.
   * 
   * This basically goes through the registered factories, and the first one that actually
   * constructs a Page based on this name wins.
   * 
   * TODO: this is arguably a stupid way for this to work, and should probably be
   * restructured to have a Map of factories by name instead. The current approach is
   * mostly a historical artifact.
   */
  def constructPage(name:String, params:ParamMap):Option[Page] = {
    val pageOpt = (Option.empty[Page] /: factories) { (opt, factory) =>
      opt match {
        case Some(page) => opt
        case None => factory.constructPageOpt(name, params)
      }
    }
    
    // If we haven't found a Page with that name, then it's naming a Thing. Go to that
    // Thing's Page *if* we're in a legit Space; otherwise, fall back to the Index.
    pageOpt orElse {
      if (DataAccess.space.isDefined)
        Some(new ThingPage(TID(name), params))
      else
        None
    }
  }
  
  def findPageFor(node:ManagedFrag[_]):Option[Page] = {
    node.findParentGadget(_.isInstanceOf[Page]).map(_.asInstanceOf[Page])
  }
  
  def updatePage(node:ManagedFrag[_]) = {
    findPageFor(node).map(_.reindex())
  }
}
