package querki.imexport.mysql

import models._
import Thing._

import querki.globals._
import querki.time._
import querki.values.{QValue, RequestContext, SpaceState}

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
  
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Core = interface[querki.core.Core]
  lazy val Time = interface[querki.time.Time]
  lazy val System = interface[querki.system.System]
  
  lazy val SystemSpace = System.State
  lazy val systemId = SystemSpace.id
  
  var _nextId = 1
  // Mapping from MySQL Primary Key to Thing OID
  var idMap = Map.empty[SQLVal[_], OID]
  // Mapping from MySQL Column to Property
  var colMap = Map.empty[ColumnInfo, AnyProp]
  // Keep track of the Properties by simple names, so that we know to insert
  // kickers when we need them
  var propNames = Set.empty[String]
  lazy val spaceId = createOID()
  var modelMap = Map.empty[TableName, OID]
  
  /**
   *  Create an Import OID for a Thing that is based on a row in the MySQL. 
   *  
   *  This will then get translated to a real OID when we create the real Space.
   *  
   *  @param importedKey The primary key of the row in MySQL, if any.
   */
  def createOID(importedKey:SQLVal[_] = NullVal):OID = {
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
      spaceId,
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
  
  // Take a name in typical SQL "word_word_word" format, and change it to Querki's
  // usual "Word Word Word" style
  def fixName(rawName:String):String = {
    val words:Array[String] = rawName.split("_").map(_.capitalize)
    words.mkString
  }
  
  def choosePropName(col:MySQLColumn, tbl:MySQLTable):String = {
    val rawName = col.col.name.v
    val name = 
      if (propNames.contains(rawName))
        s"${tbl.name.v} $rawName"
      else
        rawName
    propNames += name
    name
  }
  def buildProperty(col:MySQLColumn, tbl:MySQLTable):Option[AnyProp] = {
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
          
      def charHeuristic(size:Int) = {
        if (size < 80)
          Core.TextType
        else
          Core.LargeTextType
      }
          
      val pType:PType[_] with PTypeBuilder[_,_] = 
        // As we add more SQLTypes, make sure this keeps up:
        col.tpe match {
          case SQLInt(size:Int) => Core.IntType
          case SQLUInt(size:Int) => Core.IntType
          case SQLBigInt(size:Int) => Core.LongType
          case SQLDouble => Core.FloatType
          case SQLChar(size:Int) => charHeuristic(size) 
          case SQLVarchar(size:Int) => charHeuristic(size)
          case SQLLongtext => Core.LargeTextType
          case SQLDate => Time.QDateTime
          case SQLTimestamp => Time.QDateTime
          case _ => throw new Exception(s"Trying to build a Property for unknown SQLType ${col.tpe}")
        }
      
      // Properties are required to have unique names, at least for now, so we 
      // use the table name as a kicker when we must:
      val name = choosePropName(col, tbl)
      val qName = fixName(name)
      
      val prop =
        Property(
          createOID(),
          spaceId,
          Core.UrProp.id,
          pType.asInstanceOf[PType[Any] with PTypeBuilder[Any, Any]],
          collection,
          () => Map(Core.setName(qName)),
          DateTime.now
        )
        
      Some(prop)
    }
  }
  def buildProperties(db:MySQLDB, stateIn:SpaceState):SpaceState = {
    (stateIn /: db.tables.values) { (tblState, table) => 
      (tblState /: table.columns.values) { (colState, col) =>
        colMap.get(col.col) match {
          case Some(prop) => colState // Already created by another Table
          case None => {
            buildProperty(col, table) match {
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
  
  def buildQValue[RT](col:MySQLColumn, prop:Property[_,RT], sqlVal:SQLVal[_]):QValue = {
    val v:RT = sqlVal.v.asInstanceOf[RT]
    QValue.make(prop.cType, prop.pType, v)
  }
  
  def buildModels(db:MySQLDB, stateIn:SpaceState):SpaceState = {
    (stateIn /: db.tables.values) { (state, table) =>
      val propPairs = table.columns.values.map { col =>
        val prop = colMap(col.col)
        val default:QValue = col.defaultOpt.map(v => buildQValue(col, prop, v)).getOrElse(prop.default(stateIn))
        (prop.id, default)
      }
      
      val oid = createOID()
      modelMap += (table.name -> oid)
      
      val model = ThingState(
        oid,
        spaceId,
        Basic.SimpleThing.id,
        () => Map(propPairs.toSeq:_*)
      )
      
      state.copy(things = state.things + (oid -> model))
    }
  }
  
  def buildSpaceState(initDB:MySQLDB):SpaceState = {
    val db = preprocessDB(initDB)
    
    val initState = initialState
    val withProps = buildProperties(db, initState)
    val withModels = buildModels(db, withProps)
    
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

