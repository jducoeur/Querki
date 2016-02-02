package querki.test.functional

import querki.data.TID

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
}

/**
 * The root abstraction that corresponds to a Thing on the Server. This is f-bounded so that
 * we can have operations that operate on the actual Kind.
 */
trait TThing[T <: TThing[T]] {
  def display:String
  def tid:TID
  
  def withTID(id:String):T
}

/**
 * Represents a Space. This typically exists both as a static object, describing
 * the Space to be created, and in the State once it has been created.
 */
case class TSpace(
  // The display name we're giving this Space:
  display:String,
  // The OID of this Thing, as understood by the Client:
  tid:TID = TID("")) extends TThing[TSpace]
{
  def withTID(id:String) = copy(tid = TID(id))
}

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
  // The Spaces that *actually* exist, that we have created
  spaces:Seq[TSpace])
{
  def ->(page:QPage):State = copy(currentPage = page)
}
/**
 * The State at the beginning of time, before we've logged in or built anything.
 */
object InitialState extends State(None, LoginPage, Seq.empty)
