package querki.system

/**
 * Simple enumeration, saying whether we're in a local or production environment.
 */
sealed trait QuerkiEnv { def name: String }

object QuerkiEnv {

  /**
   * Local development machine
   */
  object Local extends QuerkiEnv { val name = "local" }

  /**
   * Test environment (eg, CI and AWS test)
   */
  object Test extends QuerkiEnv { val name = "test" }

  /**
   * Production environment
   */
  object Prod extends QuerkiEnv { val name = "prod" }

  /**
   * Fetch the current environment, based on the value of the "QUERKI_ENV" env var.
   *
   * This will throw an exception if that var is not set.
   */
  def load(): QuerkiEnv = {
    val envName = sys.env.get("QUERKI_ENV")
    envName match {
      case Some(Local.name) => Local
      case Some(Test.name)  => Test
      case Some(Prod.name)  => Prod

      case other =>
        throw new RuntimeException(s"QUERKI_ENV is '$other' -- it must be one of 'local', 'test', or 'prod'!")
    }
  }
}
