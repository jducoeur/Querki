package querki.pages

import querki.globals._

import querki.comm.URL
import querki.data.BasicThingInfo

/**
 * Convenience base class for constructing PageFactories.
 */
class PageFactoryBase(registeredName:String, const:ParamMap => Page)(implicit val ecology:Ecology) extends PageFactory with EcologyMember {
  lazy val PageManager = interface[querki.display.PageManager]
  
  def constructPageOpt(pageName:String, params:ParamMap):Option[Page] = {
    if (pageName == registeredName)
      Some(const(params))
    else
      None
  }
  
  def pageUrl(params:(String, String)*):URL = PageManager.pageUrl(registeredName, Map(params:_*))  
  
  def showPage(params:(String, String)*):Future[Page] = PageManager.showPage(registeredName, Map(params:_*))
}

class ThingPageFactoryBase(registeredName:String, const:ParamMap => Page, paramName:String)(implicit e:Ecology) 
  extends PageFactoryBase(registeredName, const) with ThingPageFactory 
{
  def pageUrl(thing:BasicThingInfo, addlParams:(String, String)*):URL = pageUrl((addlParams :+ (paramName -> thing.urlName.underlying)):_*)
  
  def showPage(thing:BasicThingInfo):Future[Page] = showPage(thing.urlName)
  
  def showPage(tid:TID):Future[Page] = PageManager.showPage(registeredName, Map(paramName -> tid.underlying))
}

class RawThingPageFactory(implicit val ecology:Ecology) extends ThingPageFactory with EcologyMember {
  lazy val PageManager = interface[querki.display.PageManager]
  
  // This factory doesn't get registered the same way, and doesn't get invoked like these.
  // TODO: the implication is that the ThingPageFactory interface shouldn't derive from PageFactory?
  def constructPageOpt(pageName:String, params:ParamMap):Option[Page] = ???
  def pageUrl(params:(String, String)*):URL = ???
  def showPage(params:(String, String)*):Future[Page] = ???
  
  def pageUrl(thing:BasicThingInfo, addlParams:(String, String)*):URL = {
    PageManager.pageUrl(thing.urlName.underlying, Map(addlParams:_*))
  }
  
  def showPage(thing:BasicThingInfo):Future[Page] = showPage(thing.urlName)
  
  def showPage(tid:TID):Future[Page] = PageManager.showPage(tid.underlying, Map.empty)
}
