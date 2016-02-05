package querki.test.functional

import querki.api.commonName
import querki.data.TID

trait FuncData { this:FuncMixin =>
  /**
   * Represents a user who exists in the test database.
   */
  case class TestUser(handle:String, display:String, password:String)
  object Admin1 extends TestUser("testadmin1", "Test Admin 1", "testing")
  
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
    val name = "root"
    
    override def is(other:QPage) = other match {
      case RootPage(otherSpace) => otherSpace is space
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
  case class ModelDesigner(model:TInstance) extends QPage {
    val name = "modelDesigner"
    
    override def titleParams = Seq(("modelName" -> model.display))
  }
  
  /**
   * The root abstraction that corresponds to a Thing on the Server. This is f-bounded so that
   * we can have operations that operate on the actual Kind.
   */
  trait TThing[T <: TThing[T]] {
    def display:String
    def tid:TID
  
    // Is there any clean way to implement this up here? It's always the same, but uses the
    // copy constructor of the children, so I don't see a way.
    def withTID(id:String):T
    
    /**
     * Ugly but occasionally useful, to get at the stringified OID of this Thing.
     */
    def oidStr = tid.underlying.drop(1)
    
    def is(other:T) = tid == other.tid
  }
  
  /**
   * Represents a Space. This typically exists both as a static object, describing
   * the Space to be created, and in the State once it has been created.
   */
  case class TSpace(
    // The display name we're giving this Space:
    display:String,
    // The OID of this Thing, as understood by the Client:
    tid:TID = TID(""),
    // The Things that have *actually* been created in this Space:
    things:Seq[TThing[_]] = Seq.empty) extends TThing[TSpace]
  {
    def withTID(id:String) = copy(tid = TID(id))
  }
  
  /**
   * Represents a normal Instance. (Also used for Models.)
   */
  case class TInstance(
    display:String,
    tid:TID = TID(""),
    model:TInstance = SimpleThing,
    isModel:Boolean = false) extends TThing[TInstance]
  {
    def withTID(id:String) = copy(tid = TID(id))
  }
  
  sealed case class TColl(tid:TID)
  object TExactlyOne extends TColl(querki.core.MOIDs.ExactlyOneOID)
  object TOptional extends TColl(querki.core.MOIDs.OptionalOID)
  object TList extends TColl(querki.core.MOIDs.QListOID)
  object TSet extends TColl(querki.core.MOIDs.QSetOID)
  
  /**
   * Enumeration of the PTypes that we can test.
   */
  sealed trait TType {
    def tid:TID
    def display:String
  }
  /**
   * Represents a single-line text field.
   */
  case object TTextType extends TType {
    def tid = querki.core.MOIDs.TextTypeOID
    def display = "Text Type"
  }
  /**
   * Represents a Tag
   * 
   * @param modelOpt The Model that this Tag should be constrained to.
   */
  case class TTagType(modelOpt:Option[TInstance]) extends TType {
    def tid = querki.tags.MOIDs.NewTagSetOID
    def display = "Tag Type"
  }
  
  /**
   * Represents a Property.
   */
  case class TProp(
    display:String,
    coll:TColl,
    tpe:TType,
    tid:TID = TID("")) extends TThing[TProp]
  {
    def withTID(id:String) = copy(tid = TID(id))
  }
  
  /**
   * The actual Simple Thing object.
   */
  object SimpleThing extends TInstance("Simple Thing", querki.basic.MOIDs.SimpleThingOID, model = null)
  
  /**
   * The Name Property.
   */
  object NameProp 
    extends TProp(commonName(_.basic.displayNameProp), TExactlyOne, TTextType, querki.basic.MOIDs.DisplayNameOID)
  
  /**
   * Represents the *current* state of the test world, including where the client
   * currently is. Most interesting functions should take this and return it.
   * 
   * You can think of this as similar to a virtual DOM in the JavaScript world. It
   * is our understanding of the state of the actual browser. Operations that alter
   * the actual browser should alter this to match.
   */
  case class State(
    // The User who we believe is currently logged in
    currentUserOpt:Option[TestUser],
    // The Page that we believe is currently showing
    currentPage:QPage,
    // The Space we're currently in
    currentSpace:Option[TSpace],
    // The Spaces that *actually* exist, that we have created
    spaces:Map[TID, TSpace])
  {
    def ->(page:QPage):State = copy(currentPage = page)
    
    /**
     * TBD: for the moment, we're identifying Spaces by display name, since the prototypes don't have
     * TIDs. Should we do something better, more guaranteed to be unique?
     */
    def getSpace(target:TSpace):TSpace = spaces.values.find(_.display == target.display).getOrElse(throw new Exception("Space ${target} hasn't been created yet?"))
    
    def updateSpace(f:TSpace => TSpace):State = {
      val space = currentSpace.getOrElse(throw new Exception(s"Trying to update the Space in $this, but there isn't one!"))
      val updated = f(space)
      copy(currentSpace = Some(updated), spaces = spaces + (updated.tid -> updated))
    }
  }
  /**
   * The State at the beginning of time, before we've logged in or built anything.
   */
  object InitialState extends State(None, LoginPage, None, Map.empty)
}