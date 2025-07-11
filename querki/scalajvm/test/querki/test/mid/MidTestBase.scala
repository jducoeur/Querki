package querki.test.mid

import org.scalatest._
import org.scalatestplus.play._
import play.api.{Application, ApplicationLoader, Environment}
import querki.globals._
import querki.system.{QuerkiApplicationLoader, QuerkiRoot}
import AllFuncs._
import org.scalatestplus.play.guice.GuiceOneAppPerTest

/**
 * Primary base trait for all "mid-level" tests. These are semi-functional tests: testing the more or less
 * real server, but with stubbed databases, at the Client-API level, with no real UI. This testing approach
 * is much faster and easier than full-fledged functional tests, but provides a somewhat less realistic
 * environment.
 *
 * This intentionally uses OneAppPerTest, not OneAppPerSuite, because the latter creates the app even if
 * we aren't running any actual tests due to -l filtering.
 */
trait MidTestBase extends PlaySpec with GuiceOneAppPerTest with EcologyMember {
  lazy val dbManager = new MidFuncDB

  /**
   * Constructs the standard "blank" initial state of a test run.
   */
  def initialState(): PreInitialState = PreInitialState.empty(this)

  override implicit def newAppForTest(testData: TestData): Application = {
    // IMPORTANT: test code runs with an alternate config file, application.test.conf, which
    // enhances the built-in one!
    val context = ApplicationLoader.createContext(
      Environment.simple(),
      Map(
        // See scenario.conf instead of here
      )
    )

    // We run setupDatabase *during* load -- after Play initializes but before the Ecology:
    QuerkiApplicationLoader._preEcologyFunc = dbManager.setupDatabase
    val app = new QuerkiApplicationLoader().load(context)

    // Fetch the actual running Ecology, so that tests can introspect into the system.
    _ecology = Some(QuerkiRoot.ecology)

    app
  }

  var _ecology: Option[Ecology] = None

  implicit def ecology: Ecology =
    _ecology.getOrElse(throw new Exception(s"Attempting to fetch the Ecology before the Application has loaded!"))

  /**
   * The standard boilerplate to set up and initialize a mid-test.
   */
  def runTest(test: TestOp[Unit]) = {
    val testOp = for {
      _ <- initState
      _ <- test
    } yield ()

    val ioa = testOp.run(initialState())
    ioa.unsafeRunSync()
  }
}
