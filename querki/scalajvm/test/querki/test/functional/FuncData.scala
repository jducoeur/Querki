package querki.test.functional

/**
 * Represents a user who exists in the test database.
 */
case class TestUser(handle:String, display:String, password:String)

/**
 * Represents a page that may be shown in the browser.
 */
sealed trait Page
case object LoginPage extends Page
case object IndexPage extends Page

/**
 * Represents the *current* state of the test world, including where the client
 * currently is. Most interesting functions should take this and return it.
 */
case class CurrentState(
  // The User who we believe is currently logged in
  currentUserOpt:Option[TestUser],
  // The Page that we believe is currently showing
  currentPage:Page)
{
  def ->(page:Page):CurrentState = copy(currentPage = page)
}

/**
 * This defines common data that are used by a variety of tests, such as
 * the test users.
 * 
 * @author jducoeur
 */
trait FuncData {
  /**
   * The original Admin user, who exists when we start these tests.
   */
  val admin1 = TestUser("testadmin1", "Test Admin 1", "testing")
}
