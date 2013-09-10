package querki.util

import java.security._
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec

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

/**
 * Represents a hash, suitable for storing passwords and such. Note that the salt
 * is not necessarily private: it is a one-time random collection of data, to keep
 * rainbow tables from working. You will need the salt in order to authenticate,
 * though.
 */
case class EncryptedHash(salt:HashInfo, hash:HashInfo) {
  override def toString = salt.toString + "." + hash.toString
}
object EncryptedHash {
  def apply(str:String):EncryptedHash = {
    str.split('.') match {
      case Array(salt, hash, _*) => EncryptedHash(salt, hash)
      case _ => throw new Exception("Malformed hash: " + str)
    }
  }
}

case class SignedHash(signature:String, salt:String, msg:String) {
  override def toString = signature + "-" + salt + "-" + msg
}
object SignedHash {
  def apply(str:String):SignedHash = {
    str.split('-') match {
      case Array(signature, salt, msg, _*) => SignedHash(signature, salt, msg)
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
 */
object Hasher {
  /**
   * Create a random 8-byte salt, to use in hashing.
   * 
   * Note that calling code shouldn't usually need to call this explicitly; it is
   * called from inside calcHash().
   */
  private def makeSalt():Array[Byte] = {
    val randomizer = SecureRandom.getInstance("SHA1PRNG")
    val salt = new Array[Byte](8)
    randomizer.nextBytes(salt)
    salt
  }
  
  lazy val iterations = Config.getInt("querki.security.hashIterations", 20000)
  
  def doCalcHash(salt:Array[Byte], original:String):EncryptedHash = {
    // The 160 here matches the SHA-1 algorithm we're using:
    val keySpec = new PBEKeySpec(original.toCharArray, salt, iterations, 160)
    val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
    EncryptedHash(HashInfo(salt), HashInfo(factory.generateSecret(keySpec).getEncoded()))    
  }
  
  /**
   * Given a text String, this returns its hashed form, and the salt that was used
   * to generate the hash.
   */
  def calcHash(original:String):EncryptedHash = doCalcHash(makeSalt, original)
  
  /**
   * Does the provided original match the hash information?
   */
  def authenticate(original:String, hash:EncryptedHash):Boolean = hash.hash.equals(doCalcHash(hash.salt.raw, original).hash)
  
  /**
   * Sign the given string, with some salt for good measure.
   * 
   * NOTE: this currently only works when the Play app is running!
   */
  def sign(original:String):SignedHash = {
    val salt = HashInfo(makeSalt).toString
    val withSalt = salt + "-" + original
    val sig = Crypto.sign(withSalt)
    SignedHash(sig, salt, original)
  }
  
  def checkSignature(hash:SignedHash):Boolean = {
    val withSalt = hash.salt + "-" + hash.msg
    val sig = Crypto.sign(withSalt)
    (sig == hash.signature) 
  }
}