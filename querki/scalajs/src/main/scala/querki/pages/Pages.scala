package querki.pages

import querki.globals._

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
  
  override def postInit() = {
    exploreFactory
    viewFactory
    createAndEditFactory
    sharingFactory
    advancedFactory
    indexFactory
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
  def showSpacePage(space:SpaceInfo) = PageManager.showPage(s"${space.urlName.underlying}", Map.empty)
  
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
  def constructPage(name:String, params:ParamMap):Page = {
    val pageOpt = (Option.empty[Page] /: factories) { (opt, factory) =>
      opt match {
        case Some(page) => opt
        case None => factory.constructPageOpt(name, params)
      }
    }
    
    // Fall back to ThingPage if nothing else claims ownership:
    pageOpt.getOrElse(new ThingPage(TID(name), params))
  }
  
  def findPageFor(node:ManagedFrag[_]):Page = {
    node.findParentGadget(_.isInstanceOf[Page]).asInstanceOf[Page]
  }
}
