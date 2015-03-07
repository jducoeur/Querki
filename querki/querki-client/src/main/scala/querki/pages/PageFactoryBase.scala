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
}

class ThingPageFactoryBase(registeredName:String, const:ParamMap => Page, paramName:String)(implicit e:Ecology) 
  extends PageFactoryBase(registeredName, const) with ThingPageFactory 
{
  def pageUrl(thing:BasicThingInfo, addlParams:(String, String)*):URL = pageUrl((addlParams :+ (paramName -> thing.urlName.underlying)):_*)
  
  def showPage(thing:BasicThingInfo):Future[Page] = showPage(thing.urlName)
  
  def showPage(tid:TID):Future[Page] = PageManager.showPage(registeredName, Map(paramName -> tid.underlying))
}
