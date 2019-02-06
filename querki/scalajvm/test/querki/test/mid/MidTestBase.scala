package querki.test.mid

import org.scalatest._
import org.scalatestplus.play._

import play.api.{Application, ApplicationLoader, Environment}
import play.api.db.Databases
import play.api.inject.guice._

import querki.db.ShardKind
import querki.globals._
import querki.system.{QuerkiApplicationLoader, QuerkiRoot}

import AllFuncs._

/**
 * Primary base trait for all "mid-level" tests. These are semi-functional tests: testing the more or less
 * real server, but with stubbed databases, at the Client-API level, with no real UI. This testing approach
 * is much faster and easier than full-fledged functional tests, but provides a somewhat less realistic
 * environment.
  *
  * This intentionally uses OneAppPerTest, not OneAppPerSuite, because the latter creates the app even if
  * we aren't running any actual tests due to -l filtering.
 */
trait MidTestBase 
  extends PlaySpec
  with OneAppPerTest
  with EcologyMember
{
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
        // Flag to allow system code to check whether we are in this mode:
        "querki.test.inmemory" -> "true",
        // For these tests, use the in-memory H2 SQL DB:
        "db.system.driver" -> "org.h2.Driver",
        "db.system.url" -> "jdbc:h2:mem:system;MODE=MYSQL",
        "db.user.driver" -> "org.h2.Driver",
        "db.user.url" -> "jdbc:h2:mem:user;MODE=MYSQL",
        "db.template.driver" -> "org.h2.Driver",
        "db.template.url" -> "jdbc:h2:mem:user;MODE=MYSQL",
        
        // Tell the Email Ecot to use the test version of the sender, which doesn't actually send
        // mail, but instead lets us inspect what has been "sent":
        "querki.mail.test" -> "true",
        // Don't bother to throttle emails significantly:
        "querki.mail.throttle" -> "10ms"
      ))
      
    // We run setupDatabase *during* load -- after Play initializes but before the Ecology:
    QuerkiApplicationLoader._preEcologyFunc = dbManager.setupDatabase
    val app = new QuerkiApplicationLoader().load(context)
    
    // Fetch the actual running Ecology, so that tests can introspect into the system.
    _ecology = Some(QuerkiRoot.ecology)    
    
    app
  }
  
  var _ecology: Option[Ecology] = None
  implicit def ecology: Ecology = _ecology.getOrElse(throw new Exception(s"Attempting to fetch the Ecology before the Application has loaded!"))
  
  /**
   * The standard boilerplate to set up and initialize a mid-test.
   */
  def runTest(test: TestOp[Unit]) = {
    val testOp = for {
      _ <- initState
      _ <- test
    }
      yield ()
      
    val ioa = testOp.run(initialState())
    ioa.unsafeRunSync()
  }
}
