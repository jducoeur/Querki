package querki.test.functional

case class TestUser(handle:String, display:String, password:String)

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