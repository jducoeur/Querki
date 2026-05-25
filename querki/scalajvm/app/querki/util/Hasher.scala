package querki.util

import java.security.SecureRandom
import play.api.libs.crypto.{CookieSigner, CookieSignerProvider}
import querki.ecology.{Ecology, PlayEcology}

/**
 * Wraps around part of a hash, to make stringifying easier. The stringifier code is
 * adapted from here:
 *
 * http://stackoverflow.com/questions/1645260/can-i-construct-a-bigint-with-any-byte-array-scala
 */
case class HashInfo(raw: Array[Byte]) {
  override def toString = new java.math.BigInteger(addByte(raw)).toString(36)

  private def addByte(ba: Array[Byte]): Array[Byte] = {
    val h = new Array[Byte](1)
    h(0) = 0x01
    h ++ ba
  }

  override def equals(other: Any) = {
    other match {
      case HashInfo(otherRaw) => raw.sameElements(otherRaw)
      case _                  => false
    }
  }
}

object HashInfo {

  implicit def apply(str: String): HashInfo = {
    HashInfo(stripByte(new java.math.BigInteger(str, 36).toByteArray))
  }

  private def stripByte(ba: Array[Byte]): Array[Byte] = ba.slice(1, ba.size)
}

case class SignedHash(
  signature: String,
  salt: String,
  msg: String,
  sep: Char
) {
  override def toString = signature + sep + salt + sep + msg
}

object SignedHash {

  def apply(
    str: String,
    sep: Char
  ): SignedHash = {
    str.split(sep) match {
      case Array(signature, salt, msg, _*) => SignedHash(signature, salt, msg, sep)
      case _                               => throw new Exception("Bad signature string: " + str)
    }
  }
}

/**
 * This is a hash-generation service, originally adapted from this page:
 *
 * http://www.javacodegeeks.com/2012/05/secure-password-storage-donts-dos-and.html
 *
 * But this no longer has much to do with our password hashing; instead, it's mainly used to
 * sign invitations and suchlike.
 *
 * IMPORTANT: As of this writing, this uses PBKDF2. That's not half-bad, and fine
 * for many purposes. (Eg, invitation links.)
 *
 * TODO: this should become an Ecot! Note that parts of it have been pulled out into
 * querki.security.EncryptionEcot. The remainder is currently Play-dependent; we should
 * decide if we are willing/able to strip out the Play dependency and put it all in
 * there, unifying the code.
 */
object Hasher {

  /**
   * Create a random 8-byte salt, to use in hashing.
   *
   * Note that calling code shouldn't usually need to call this explicitly; it is
   * called from inside calcHash().
   */
  def makeSalt(): Array[Byte] = {
    val randomizer = SecureRandom.getInstance("SHA1PRNG")
    val salt = new Array[Byte](8)
    randomizer.nextBytes(salt)
    salt
  }

  /**
   * Gets the standard Play CookieSigner, for signing things.
   *
   * TODO: this isn't quite right. This is only intended for signing *cookies*, and we're mostly signing other
   * things. We got here because we used to be using the now-deprecated Crypto.sign(), which went away at Play 2.6.
   * It's not tragic, but it's a bit inappropriate -- we should switch to using an algorithm that is more apt for
   * the use cases we're putting it to.
   */
  private def signer(implicit ecology: Ecology): CookieSigner = {
    val cookieSignerProvider = PlayEcology.playApi[CookieSignerProvider]
    cookieSignerProvider.get
  }

  /**
   * Sign the given string, with some salt for good measure.
   *
   * sep is the character to use as the separator between the elements of the
   * signed result. Choose a separator that makes sense for your problem domain:
   * it should be a character that you are sure won't be in the original value.
   *
   * NOTE: this currently only works when the Play app is running! We should think about
   * whether that's a fatal weakness for testing.
   */
  def sign(
    original: String,
    sep: Char
  )(implicit
    ecology: Ecology
  ): SignedHash = {
    val salt = HashInfo(makeSalt).toString
    val withSalt = salt + "-" + original
    val sig = signer.sign(withSalt)
    SignedHash(sig, salt, original, sep)
  }

  def checkSignature(hash: SignedHash)(implicit ecology: Ecology): Boolean = {
    val withSalt = hash.salt + "-" + hash.msg
    val sig = signer.sign(withSalt)
    (sig == hash.signature)
  }
}
