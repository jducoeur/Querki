package querki.test.functional

import anorm._

import querki.db._
import ShardKind._

/**
 * Mix-in trait that defines the database primitives for functional testing.
 * 
 * @author jducoeur
 */
trait FuncDB {
  
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
    }
    
    QDB(User) { implicit conn =>
      SQL("CREATE TABLE OIDNexter LIKE test_system_template.OIDNexter").execute()
    }
  }

}