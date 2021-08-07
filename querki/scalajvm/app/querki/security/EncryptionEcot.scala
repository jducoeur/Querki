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
case class EncryptedHash(
  salt: HashInfo,
  hash: HashInfo
) {
  override def toString = salt.toString + "." + hash.toString
}

object EncryptedHash {

  def apply(str: String): EncryptedHash = {
    str.split('.') match {
      case Array(salt, hash, _*) => EncryptedHash(salt, hash)
      case _                     => throw new Exception("Malformed hash: " + str)
    }
  }
}

/**
 * These functions are intentionally separated out from EncryptionEcot so that you can
 * call them from the console command line, if you need to, eg, muck with encrypted
 * values in the DB. You should '''not''' call these functions directly from code:
 * go through the Ecot as normal instead.
 */
object EncryptionUtil {

  def doCalcHash(
    salt: Array[Byte],
    original: String,
    iterations: Int
  ): EncryptedHash = {
    // TODO: we should likely switch to scrypt here:
    // The 160 here matches the SHA-1 algorithm we're using:
    val keySpec = new PBEKeySpec(original.toCharArray, salt, iterations, 160)
    val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
    EncryptedHash(HashInfo(salt), HashInfo(factory.generateSecret(keySpec).getEncoded()))
  }

  def calcHash(
    original: String,
    iterations: Int
  ): String = doCalcHash(Hasher.makeSalt, original, iterations).toString

  def authenticate(
    original: String,
    rawHash: String,
    iterations: Int
  ): Boolean = {
    val hash = EncryptedHash(rawHash)
    hash.hash.equals(EncryptionUtil.doCalcHash(hash.salt.raw, original, iterations).hash)
  }

}

class EncryptionEcot(e: Ecology) extends QuerkiEcot(e) with Encryption {

  lazy val iterations = Config.getInt("querki.security.hashIterations", 20000)

  def calcHash(original: String): String = EncryptionUtil.calcHash(original, iterations)

  def authenticate(
    original: String,
    rawHash: String
  ): Boolean =
    EncryptionUtil.authenticate(original, rawHash, iterations)

}
