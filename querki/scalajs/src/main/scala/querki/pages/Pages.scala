package querki.pages

import querki.globals._
import querki.search.SearchResultsPage

class PagesEcot(e:Ecology) extends ClientEcot(e) with Pages {
  
  def implements = Set(classOf[Pages])
  
  override def postInit() = {
    // PageFactory for the ExplorePage, since there isn't another good package to stick it
    // into:
    registerStandardFactory("_explore", { (params) => new ExplorePage(params) })
  }
  
  private var factories = Seq.empty[PageFactory]
  
  def registerFactory(factory:PageFactory):Unit = {
    factories :+= factory
  }
  
  def registerStandardFactory(registeredName:String, const:ParamMap => Page) = {
    registerFactory(new PageFactory {
      def constructPageOpt(pageName:String, params:ParamMap):Option[Page] = {
        if (pageName == registeredName)
          Some(const(params))
        else
          None
      }
    })    
  }
  
  /**
   * The big hardcoded factory for Pages.
   * 
   * This is a bit inelegant and coupled, but there is no great answer, and it doesn't
   * actually violate DRY.
   * 
   * TODO: a better way for this to work would be for each Page to have a factory, and
   * have its corresponding Ecot register that factory in postInit(). That probably implies
   * moving each Page to the relevant package, but that seems more and more correct anyway.
   */
  def constructPage(name:String, params:ParamMap):Page = {
    val pageOpt = (Option.empty[Page] /: factories) { (opt, factory) =>
      opt match {
        case Some(page) => opt
        case None => factory.constructPageOpt(name, params)
      }
    }
    
    // Fall back to ThingPage if nothing else claims ownership:
    pageOpt.getOrElse(new ThingPage(name, params))
  }
  
}
