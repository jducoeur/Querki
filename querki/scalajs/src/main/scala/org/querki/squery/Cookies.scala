package org.querki.squery

import scala.scalajs.js
import js.{Date, URIUtils}
import org.scalajs.dom

/**
 * Functions for manipulating the cookies that are set in the DOM.
 * 
 * Note this code, which seems to be the most complete solution, but is kind of insane:
 * 
 *   https://developer.mozilla.org/en-US/docs/Web/API/Document/cookie/Simple_document.cookie_framework
 *   
 * This provides some inspiration, but we're going to make the simplifying assumption (for now)
 * that we are reading the same cookies we are writing, so we don't have to be as forgiving about
 * formatting. (And we aren't going to abuse regular expressions so horribly.) PRs welcome to make
 * this more forgiving.
 * 
 * At least for now, we are assuming that all cookies use the root path and current domain, which is 
 * typically appropriate for SPAs.
 * 
 * Note that both keys *and* values get encoded, so that things should be generally Unicode-safe.
 * 
 * NOTE: this code is not yet tested!
 */
object Cookies {
  @inline def encode(s:String) = URIUtils.encodeURIComponent(s)
  @inline def decode(s:String) = URIUtils.decodeURIComponent(s)
  
  /**
   * Far-future expiration date, to pass in as the expires parameter on set().
   */
  val never = Some(new Date(9999))

  /**
   * Fetch the named cookie, if it exists.
   */
  def get(key:String):Option[String] = {
    val keySearch = encode(key) + "="
    decode(dom.document.cookie)
      .split(';')
      .find(_.startsWith(keySearch))
      .map(_.substring(keySearch.length))
  }
  
  private implicit class cookieBuilder(s:String) {
    def withAttr[T](name:String, vOpt:Option[T], m:T => String):String = {
      vOpt match {
        case Some(v) => {
          val suffix = m(v) match {
            case "" => ""
            case vDisp => s"=$vDisp"
          }
          s + s"; $name$suffix"
        }
        case None => s
      }
    }
  }
  
  def set(key:String, value:String, expires:Option[Date] = None, secure:Option[Boolean] = None, maxAge:Option[Long] = None) = {
    dom.document.cookie =
      encode(key) + 
        "=" + 
        encode(value).
        withAttr[Date]("expires", expires, _.toUTCString).
        withAttr[Long]("max-age", maxAge, _.toString).
        // secure is just a flag:
        withAttr[Boolean]("secure", secure, _ => "")
  }
  
  def clear(key:String) = set(key, "", expires = Some(new Date(0)))
}
