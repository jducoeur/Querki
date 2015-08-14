package querki.imexport.mysql

import models.OID

import querki.globals._
import querki.values.{RequestContext, SpaceState}

/**
 * Parses a MySQL dumpfile into an in-memory data structure.
 * 
 * This doesn't try to be excessively brilliant. It can't deal with arbitrary SQL, doesn't even
 * begin to try to be comprehensive, and undoubtedly has lots of edge cases. But it's an ongoing
 * project to read *enough* MySQL to work for Querki import.
 * 
 * @author jducoeur
 */
class MySQLImport(rc:RequestContext)(implicit val ecology:Ecology) extends EcologyMember {
  import MySQLParse._
  import MySQLProcess._
  
  lazy val System = interface[querki.system.System]
  
  lazy val SystemSpace = System.State
  lazy val systemId = SystemSpace.id
  
  var _nextId = 1
  // Mapping from MySQL Primary Key to Thing OID
  var idMap = Map.empty[SQLVal, OID]
  // Mapping from MySQL Column to Property
  var colMap = Map.empty[ColumnName, AnyProp]
  
  /**
   *  Create an Import OID for a Thing that is based on a row in the MySQL. 
   *  
   *  This will then get translated to a real OID when we create the real Space.
   *  
   *  @param importedKey The primary key of the row in MySQL, if any.
   */
  def createOID(importedKey:SQLVal = NullVal):OID = {
    val oid = OID(1, _nextId)
    _nextId += 1
    if (importedKey != NullVal)
      idMap += (importedKey -> oid)
    oid
  }
  
  /**
   * Look at the primary key for this table, and see whether it belongs in the Space.
   */
  def checkPrimaryKey(table:MySQLTable):MySQLTable = {
    table.primaryKey match {
      case Some(key) => {
        val col = table.columns(key)
        if (col.clauses.contains(SQLAutoIncrement)) {
          // It's a standard auto-incrementing ID, which means it almost certainly
          // contains no interesting semantic info. So omit it:
          table.copy(columns = table.columns + (key -> col.copy(generateProp = false)))
        } else
          table
      }
      case _ => table
    }
  }
  
  def withConstraints(table:MySQLTable):MySQLTable = {
    (table /: table.constraints) { (tbl, constraint) =>
      val col = tbl.columns(constraint.localCol).copy(rawConstraint = Some(constraint))
      tbl.copy(columns = tbl.columns + (col.name -> col))
    }
  }
  
  /**
   * Do any heuristic pre-processing before we begin to build the Space.
   */
  def preprocessDB(db:MySQLDB):MySQLDB = {
    (db /: db.tables) { (dba, tablePair) =>
      val (name, table) = tablePair
      val withFixedKey = checkPrimaryKey(table)
      val withConstr = withConstraints(withFixedKey)
      db.copy(tables = db.tables + (name -> withConstr))
    }
  }
//  
//  def initialSpace:SpaceState = {
//    SpaceState(
//      createOID(),
//      systemId,
//      () => emptyProps,
//      rc.requesterOrAnon.mainIdentity.id,
//      name.get,
//      DateTime.now,
//      Some(SystemSpace),
//      Map.empty,
//      Map.empty,
//      Map.empty,
//      Map.empty,
//      None
//    )
//  }
  
  def buildSpaceState(initDB:MySQLDB):SpaceState = {
    val db = preprocessDB(initDB)
    
//    val initState = 
    
    ???
  }


  /* ***************************************************************
   * 
   * Main top-level function.
   * 
   */
  
  def readDumpfile(mySQL:String):SpaceState = {
    // First, do the raw parse of the file, which results in a series of statements:
    val statements = MySQLParse(mySQL)
    
    // Now, go through the statements in order, and build a MySQLDB out of them:
    val db = processStmts(statements)
    
    buildSpaceState(db)
  }
}

