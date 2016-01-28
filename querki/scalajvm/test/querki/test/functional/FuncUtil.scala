package querki.test.functional

/**
 * Common utility operations for the tests.
 * 
 * @author jducoeur
 */
trait FuncUtil { this:FuncMixin =>
  def loginAs(user:TestUser) = {
    textField("name").value = user.handle
    pwdField("password").value = user.password
    click on "login_button"
    eventually { pageTitle should be ("Your Spaces") }
  }
}
