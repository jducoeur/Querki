package querki.test.mid

import org.scalatest._
import org.scalatestplus.play._

import play.api.{Application, ApplicationLoader, Environment}
import play.api.db.Databases
import play.api.inject.guice._

import querki.db.ShardKind
import querki.globals._
import querki.system.{QuerkiApplicationLoader, QuerkiRoot}

/**
 * Primary base trait for all "mid-level" tests. These are semi-functional tests: testing the more or less
 * real server, but with stubbed databases, at the Client-API level, with no real UI. This testing approach
 * is much faster and easier than full-fledged functional tests, but provides a somewhat less realistic
 * environment.
 */
trait MidTestBase 
  extends PlaySpec
  with OneAppPerSuite
{
  lazy val dbManager = new MidFuncDB
  
  def spew(msg: => String) = QLog.spew(msg)
  
  override implicit lazy val app: Application = {
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
        // Tell the Email Ecot to use the test version of the sender, which doesn't actually send
        // mail, but instead lets us inspect what has been "sent":
        "querki.mail.test" -> "true",
        // Use the in-memory Akka Persistence driver:
        "akka.persistence.journal.plugin" -> "inmemory-journal",
        "akka.persistence.snapshot-store.plugin" -> "inmemory-snapshot-store"
      ))
      
    // We run setupDatabase *during* load -- after Play initializes but before the Ecology:
    QuerkiApplicationLoader._preEcologyFunc = dbManager.setupDatabase
    val app = new QuerkiApplicationLoader().load(context)
    
    // Fetch the actual running Ecology, so that tests can introspect into the system.
    ecology = QuerkiRoot.ecology    
    
    app
  }
  
  var ecology: Ecology = null
}
