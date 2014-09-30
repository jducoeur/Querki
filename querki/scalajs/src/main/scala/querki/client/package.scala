package querki

import upickle._
import autowire._

import querki.globals._

package object client {
  /**
   * This special interface provides the Autowire interface around any given API. The server side
   * should be properly hooked up to apiRequest() on that end.
   */
  trait Client extends EcologyInterface with autowire.Client[String, upickle.Reader, upickle.Writer]
}
