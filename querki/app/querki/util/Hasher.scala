package querki.util

import java.security.SecureRandom

import play.api.libs.Crypto

/**
 * Wraps around part of a hash, to make stringifying easier. The stringifier code is
 * adapted from here:
 * 
 * http://stackoverflow.com/questions/1645260/can-i-construct-a-bigint-with-any-byte-array-scala
 */
case class HashInfo(raw:Array[Byte]) {
  override def toString = new java.math.BigInteger(addByte(raw)).toString(36)
  
  private def addByte(ba: Array[Byte]): Array[Byte] = {
    val h = new Array[Byte](1)
    h(0) = 0x01
    h ++ ba
  }
  
  override def equals(other:Any) = {
    other match {
      case HashInfo(otherRaw) => raw.sameElements(otherRaw)
      case _ => false
    }
  }
}
object HashInfo {
  implicit def apply(str:String):HashInfo = {
    HashInfo(stripByte(new java.math.BigInteger(str, 36).toByteArray))
  }
  
  private def stripByte(ba: Array[Byte]): Array[Byte] = ba.slice(1,ba.size)
}

case class SignedHash(signature:String, salt:String, msg:String, sep:Char) {
  override def toString = signature + sep + salt + sep + msg
}
object SignedHash {
  def apply(str:String, sep:Char):SignedHash = {
    str.split(sep) match {
      case Array(signature, salt, msg, _*) => SignedHash(signature, salt, msg, sep)
      case _ => throw new Exception("Bad signature string: " + str)
    }
  }
}

/**
 * This is a hash-generation service, adapted from this page:
 * 
 * http://www.javacodegeeks.com/2012/05/secure-password-storage-donts-dos-and.html
 * 
 * IMPORTANT: As of this writing, this uses PBKDF2. That's not half-bad, and fine
 * for many purposes. (Eg, invitation links.) But for hard-core password storage,
 * we may want to upgrade to scrypt. That's more expensive to compute, but current
 * theory is that it's *probably* more secure, since it is even harder to crack.
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
  def makeSalt():Array[Byte] = {
    val randomizer = SecureRandom.getInstance("SHA1PRNG")
    val salt = new Array[Byte](8)
    randomizer.nextBytes(salt)
    salt
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
  def sign(original:String, sep:Char):SignedHash = {
    val salt = HashInfo(makeSalt).toString
    val withSalt = salt + "-" + original
    val sig = Crypto.sign(withSalt)
    SignedHash(sig, salt, original, sep)
  }
  
  def checkSignature(hash:SignedHash):Boolean = {
    val withSalt = hash.salt + "-" + hash.msg
    val sig = Crypto.sign(withSalt)
    (sig == hash.signature) 
  }
}