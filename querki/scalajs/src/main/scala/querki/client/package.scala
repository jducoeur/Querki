package querki

import upickle.default._
import autowire._

import querki.globals._

package object client {
  /**
   * This special interface provides the Autowire interface around any given API. The server side
   * should be properly hooked up to apiRequest() on that end.
   * 
   * At least for the time being, any file using Client[API] *must* import autowire._ -- there is
   * some pretty fancy implicit magic involved that I haven't managed to fold into globals yet.
   */
  trait Client extends EcologyInterface with autowire.Client[String, upickle.default.Reader, upickle.default.Writer] {
    def translateServerException(ex: Throwable): Nothing
  }
}
