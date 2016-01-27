package querki.test

/**
 * This is a place to put little, temporary tests. Note that these should normally be
 * marked ignore when checked in.
 * 
 * @author jducoeur
 */
class ScratchTests extends QuerkiTests
{
  "I" should {
    "be able to get a password" ignore {
      val Encryption = interface[querki.security.Encryption]
      
      val plaintext = "testing"
      val hash = Encryption.calcHash(plaintext)
      val auth = Encryption.authenticate(plaintext, hash)
      println(s"""The encrypted form of $plaintext is '$hash'; authenticated = $auth""")
    }
  }
}
