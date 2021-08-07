package querki

import querki.globals._

/**
 * @author jducoeur
 */
package object local {

  trait Messages {
    def getPackage(name: String): Messages

    def msg(
      name: String,
      params: (String, String)*
    ): String
  }

  trait Localization extends EcologyInterface {

    /**
     * This will resolve when we are ready to do localization.
     *
     * TODO: in principle, this is a workaround for the fact that init() is all synchronous. We really
     * want a proper async init scheme here, but that will require Ecology enhancements.
     */
    def ready: Future[Unit]

    /**
     * Fetch a named message package.
     */
    def messages(name: String): Messages
//
//    /**
//     * Fetch the named message text.
//     */
//    def msg(name:String):String
  }
}
