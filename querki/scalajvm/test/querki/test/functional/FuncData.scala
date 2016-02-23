package querki.test.functional

import org.openqa.selenium.WebElement

object TID {
  def apply(s:String):TID = s
}

trait FuncData { this:FuncMixin =>
  /**
   * Represents a user who exists in the test database.
   */
  case class TestUser(handle:String, display:String, password:String)
  object Admin1 extends TestUser("testadmin1", "Test Admin 1", "testing")
  
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
    def oidStr = tid.drop(1)
    
    def is(other:TThing[_]) = tid == other.tid
    
    /**
     * This is a craptastic definition of equality, but so far I don't have a better one without
     * mutating things.
     */
    def matches(other:TThing[_]) = display == other.display
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
    // The url of this Space, filled in once it has been created:
    url:String = "",
    // The Things that have *actually* been created in this Space:
    things:Map[TID, TInstance] = Map.empty,
    // The Properties that have actually been created:
    props:Map[TID, TProp[_]] = Map.empty) extends TThing[TSpace]
  {
    def withTID(id:String) = copy(tid = TID(id))
    
    def +(instance:TInstance) = copy(things = things + (instance.tid -> instance))
    
    def lookupIn[T <: TThing[T]](t:T, coll:Map[TID, T]):T = {
      if (t.tid.length == 0)
        // No OID -- we're trying to match against a prototype, so we have to just match by name:
        coll.values.find(_ matches t).getOrElse(fail(s"Couldn't find $t among the created ones by name!"))
      else
        // There's an OID provided, so this is easier:
        coll.get(t.tid).getOrElse(fail(s"Couldn't find $t among the created ones by oid!"))
    }
    
    def thing(t:TInstance):TInstance = {
      lookupIn(t, things)
    }
    
    def spaceThing:TInstance = {
      things.get(tid).getOrElse(fail(s"Couldn't find the spaceThing for $display"))
    }
    
    def prop[TPE <: TType](p:TProp[TPE]):TProp[TPE] = {
      // TODO: Why do I need to duplicate this code? This is evil and wrong. How do we call lookupIn()
      // successfully here?
      if (p.tid.length == 0)
        // No OID -- we're trying to match against a prototype, so we have to just match by name:
        props.values.find(_ matches p).getOrElse(fail(s"Couldn't find $p among the created ones by name!")).asInstanceOf[TProp[TPE]]
      else
        // There's an OID provided, so this is easier:
        props.get(p.tid).getOrElse(fail(s"Couldn't find $p among the created ones by oid!")).asInstanceOf[TProp[TPE]]
    }
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
  
  lazy val ExactlyOne = ICore.ExactlyOne
  lazy val Optional = ICore.Optional
  lazy val QList = ICore.QList
  lazy val QSet = ICore.QSet

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
    
    def thing(t:TInstance):TInstance = {
      currentSpace.getOrElse(fail(s"Trying to find a Thing, but we're not in a Space!")).thing(t)
    }
    
    def prop[TPE <: TType](p:TProp[TPE]):TProp[TPE] = {
      currentSpace.getOrElse(fail(s"Trying to find a Prop, but we're not in a Space!")).prop(p)
    }
    
    def tid(t:TInstance):TID = {
      val original = t.tid
      if (original.length > 0)
        original
      else
        thing(t).tid
    }
  }
  /**
   * The State at the beginning of time, before we've logged in or built anything.
   */
  object InitialState extends State(None, LoginPage, None, Map.empty)
}