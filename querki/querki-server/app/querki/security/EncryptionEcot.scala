package querki.security

import java.security._
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec

import querki.ecology._
import querki.util._

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

class EncryptionEcot(e:Ecology) extends QuerkiEcot(e) with Encryption {
  
  lazy val iterations = Config.getInt("querki.security.hashIterations", 20000)
  
  def doCalcHash(salt:Array[Byte], original:String):EncryptedHash = {
    // The 160 here matches the SHA-1 algorithm we're using:
    val keySpec = new PBEKeySpec(original.toCharArray, salt, iterations, 160)
    val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
    EncryptedHash(HashInfo(salt), HashInfo(factory.generateSecret(keySpec).getEncoded()))    
  }
  
  def calcHash(original:String):String = doCalcHash(Hasher.makeSalt, original).toString
  
  def authenticate(original:String, rawHash:String):Boolean = {
    val hash = EncryptedHash(rawHash)
    hash.hash.equals(doCalcHash(hash.salt.raw, original).hash)
  }

}