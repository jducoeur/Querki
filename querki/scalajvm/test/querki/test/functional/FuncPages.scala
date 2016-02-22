package querki.test.functional

/**
 * @author jducoeur
 */
trait FuncPages { this:FuncMixin =>
  
  /**
   * Represents a page that may be shown in the browser.
   */
  sealed trait QPage {
    def name:String
    
    def titleParams:Seq[(String, String)] = Seq.empty
    
    /**
     * Template-friendly version of ==
     */
    def is(other:QPage):Boolean = name == other.name
  }
  /**
   * Solely for test specification purposes, this means "I don't care". Don't use this unless you've
   * thought through what it means -- so far, the only use I've found is for a test suite that doesn't
   * do anything but contain sub-tests.
   */
  object AnyPage extends QPage {
    def name = "Not an actual page"
    
    override def is(other:QPage) = true
  }
  /**
   * The root page of Querki, which you see only when you aren't logged in.
   */
  object LoginPage extends QPage {
    val name = "login"
  }
  /**
   * The "Your Spaces" page, which you only see if you *are* logged in.
   */
  object IndexPage extends QPage {
    val name = "index"
  }
  /**
   * The "Create a New Space" page.
   */
  object CreateSpace extends QPage {
    val name = "createSpace"
  }
  /**
   * The root page of some Space. If this is showing, the Space had better be in the State.
   */
  case class RootPage(space:TSpace) extends QPage {
    val name = "thing"
    
    override def titleParams = Seq(("thingName" -> space.display))
    
    override def is(other:QPage) = other match {
      case RootPage(otherSpace) => otherSpace matches space
      case _ => false
    }
  }
  /**
   * The page for some Thing.
   */
  case class ThingPage[T <: TThing[_]](thing:T) extends QPage {
    val name = "thing"
    
    override def titleParams = Seq(("thingName" -> thing.display))
    
    override def is(other:QPage) = other match {
      case ThingPage(otherThing) => otherThing matches thing
      case _ => false
    }
  }
  /**
   * The page for some Tag.
   */
  case class TagPage(tag:String) extends QPage {
    val name = "thing"
    
    override def titleParams = Seq(("thingName" -> tag))
    
    override def is(other:QPage) = other match {
      case TagPage(otherTag) => otherTag matches tag
      case _ => false
    }
  }
  /**
   * The page for creating Instances.
   */
  case class CreateAndEdit[T <: TThing[T]](model:T) extends QPage {
    val name = "createAndEdit"
    
    override def titleParams = Seq(("modelName" -> model.display))
  }
  case class ModelDesigner[T <: TThing[T]](model:T, newlyCreated:Boolean = false) extends QPage {
    val name = "modelDesigner"
    
    // HACK
    val prefix = 
      model match {
        case thing:TInstance if (thing.isModel) => "Designing Model"
        case _ => "Editing"
      }
    override def titleParams = Seq(
      ("modelName" -> { if (newlyCreated) model.tid.toString else model.display }), 
      ("prefix" -> prefix)
    )
  }
  
  case class Search(query:String) extends QPage {
    val name = "search"
    
    override def titleParams = Seq(("query" -> query))
  }

}