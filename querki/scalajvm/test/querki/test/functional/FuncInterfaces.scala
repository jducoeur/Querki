package querki.test.functional

/**
 * This defines all of the EcologyInterfaces being used in the Functional Tests. (Which will eventually be
 * a large fraction of them.)
 * 
 * @author jducoeur
 */
trait FuncInterfaces { this:FuncMixin =>
  
  // Interfaces that are used in the tests. Note that we mark all of these hooks into the
  // running engine with the "I" prefix, to distinguish them. Note also that they must
  // *only* be accessed from lazy vals!
  lazy val IBasic = interface[querki.basic.Basic]
  lazy val ICore = interface[querki.core.Core]
  lazy val IEmailInspector = interface[querki.email.TestEmailInspector]
  lazy val ILinks = interface[querki.links.Links]
  lazy val IRoles = interface[querki.security.Roles]
  lazy val ITags = interface[querki.tags.Tags]
  lazy val IUI = interface[querki.html.HtmlUI]
}
