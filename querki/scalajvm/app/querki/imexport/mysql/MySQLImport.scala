package querki.imexport.mysql

import models._
import Thing._

import querki.globals._
import querki.time._
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
class MySQLImport(rc:RequestContext, name:String)(implicit val ecology:Ecology) extends EcologyMember {
  import MySQLParse._
  import MySQLProcess._
  
  lazy val Core = interface[querki.core.Core]
  lazy val System = interface[querki.system.System]
  
  lazy val SystemSpace = System.State
  lazy val systemId = SystemSpace.id
  
  var _nextId = 1
  // Mapping from MySQL Primary Key to Thing OID
  var idMap = Map.empty[SQLVal, OID]
  // Mapping from MySQL Column to Property
  var colMap = Map.empty[ColumnInfo, AnyProp]
  
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
   * Look at each column in this Table, and see whether it gets generated.
   */
  def checkPropGeneration(table:MySQLTable):MySQLTable = {
    // Go through the columns and see if there are any to be omitted:
    (table /: table.columns.values) { (tbl, col) =>
      def columnOmitted() = tbl.copy(columns = table.columns + (col.name -> col.copy(generateProp = false)))
      
      if (tbl.primaryKey.isDefined && (col.name == tbl.primaryKey.get) && col.clauses.contains(SQLAutoIncrement)) {
        // It's a standard auto-incrementing ID, which means it almost certainly
        // contains no interesting semantic info. So omit it:
        columnOmitted()
      } else if (col.tpe == SQLTimestamp && col.clauses.contains(SQLOnUpdate(SQLCurrentTimestamp))) {
        // This is an update timestamp, which is automatic in Querki; omit it
        // TODO: we should import the existing values, and slam mod_time to match them
        columnOmitted()
      } else
        tbl
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
      val withGeneration = checkPropGeneration(table)
      val withConstr = withConstraints(withGeneration)
      db.copy(tables = db.tables + (name -> withConstr))
    }
  }
  
  def initialState:SpaceState = {
    SpaceState(
      createOID(),
      systemId,
      () => emptyProps,
      rc.requesterOrAnon.mainIdentity.id,
      name,
      DateTime.now,
      Some(SystemSpace),
      Map.empty,
      Map.empty,
      Map.empty,
      Map.empty,
      None
    )
  }
  
  def buildProperty(col:MySQLColumn):Option[AnyProp] = {
    if (!col.generateProp)
      // We are intentionally not generating this column. This happens in cases like autoincremented IDs
      // and update timestamps.
      None
    else {
      // TODO: detect join tables, and build a List if found. But that'll require significant and fairly
      // smart tweaking.
      val collection = 
        if (col.nullable)
          Core.Optional
        else
          Core.ExactlyOne
          
      val pType = 
        // As we add more SQLTypes, make sure this keeps up:
        col.tpe match {
          case SQLInt(size:Int) => 
          case SQLUInt(size:Int) => 
          case SQLBigInt(size:Int) => 
          case SQLDouble => 
          case SQLChar(size:Int) => 
          case SQLVarchar(size:Int) => 
          case SQLLongtext => 
          case SQLDate => 
          case SQLTimestamp => 
          case _ => throw new Exception(s"Trying to build a Property for unknown SQLType ${col.tpe}")
        }
      ???
    }
  }
  def buildProperties(db:MySQLDB, stateIn:SpaceState):SpaceState = {
    (stateIn /: db.tables.values) { (tblState, table) => 
      (tblState /: table.columns.values) { (colState, col) =>
        colMap.get(col.col) match {
          case Some(prop) => colState // Already created by another Table
          case None => {
            buildProperty(col) match {
              case Some(prop) => {
                colMap += (col.col -> prop)
                colState.copy(spaceProps = colState.spaceProps + (prop.id -> prop))
              }
              case None => colState
            }
          }
        }
      }
    }
  }
  
  def buildSpaceState(initDB:MySQLDB):SpaceState = {
    val db = preprocessDB(initDB)
    
    val initState = initialState
    val withProps = buildProperties(db, initState)
    
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

