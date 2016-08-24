package querki.test.functional

import scala.sys.process._

import anorm._

import querki.db._
import ShardKind._
import querki.globals._
import querki.system.TOSModule

/**
 * Mix-in trait that defines the database primitives for functional testing.
 * 
 * @author jducoeur
 */
trait FuncDB { this:FuncMixin =>
  
  // TEMP: this will be able to go away once we're firmly using Cassandra:
  lazy val cassandraEnabled = true
  
  // This drops the existing test databases, and builds fresh ones based on test_system_template.
  // Note that, at the end of a test run, the test DBs will be left intact for forensic inspection.
  def setupDatabase() = {
    // First, create the test databases. We use the Template DB as the connection while we're doing so:
    QDB(Template) { implicit conn =>
      SQL("DROP DATABASE IF EXISTS test_system").execute()
      SQL("DROP DATABASE IF EXISTS test_user").execute()
      SQL("CREATE DATABASE test_system").execute()
      SQL("CREATE DATABASE test_user").execute()
    }
    
    QDB(System) { implicit conn =>
      def cmd(str:String) = SQL(str).execute()
      def makeTable(name:String) = {
        cmd(s"CREATE TABLE $name LIKE test_system_template.$name")
        cmd(s"INSERT INTO $name SELECT * FROM test_system_template.$name")
      }
      makeTable("Apps")
      makeTable("Identity")
      makeTable("OIDNexter")
      makeTable("SpaceMembership")
      makeTable("Spaces")
      makeTable("User")
      
      // Mark any pre-existing members as being up-to-date on the Terms of Service:
      cmd(s"UPDATE User SET tosVersion = ${TOSModule.currentVersion.version}")
    }
    
    QDB(User) { implicit conn =>
      SQL("CREATE TABLE OIDNexter LIKE test_system_template.OIDNexter").execute()
      SQL("INSERT INTO OIDNexter SELECT * FROM test_system_template.OIDNexter").execute()
    }
  }

  /**
   * IMPORTANT: the functional tests assume that ccm is installed locally! See
   *
   *   https://github.com/pcmanus/ccm
   *
   * This stops the current Cassandra test cluster, and builds a fresh one named ftst.  
   */
  def setupCassandra() = {
    if (cassandraEnabled) {
      spew("Setting up Cassandra cluster ftst...")
      
      // Stop the current cluster. We intentionally ignore the return code -- this will return an
      // error if there is no current cluster, and we don't care.
      "ccm stop" !
      
      // Again, this will error if there is no such cluster, and that's fine.
      "ccm remove ftst" !
  
      // This one we actually care about:
      if (("ccm create ftst -v 3.0.6 -n 3 -s" !) != 0)
        throw new Exception(s"Failed to create a fresh Cassandra cluster!")
      
      spew("... cluster created")
    }
  }
  
  /**
   * At the end of the test, this goes back to the conventional cluster.
   * 
   * TODO: obviously, this needs to be config-parameterized to deal with the current
   * "conventional" cluster for this machine.
   */
  def teardownCassandra() = {
    if (cassandraEnabled) {
      spew("Shutting down Cassandra cluster...")
      
      val ret = "ccm stop" #&&
      "ccm remove ftst" #&&
      "ccm switch dev" #&&
      "ccm start" !
      
      if (ret != 0)
        throw new Exception(s"Failed to switch back to the normal Cassandra cluster -- return code $ret!")
      
      spew("... switched back to normal test cluster.")
    }
  }
}
