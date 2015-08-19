package querki.imexport.mysql

import scala.annotation.tailrec

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
  lazy val Editor = interface[querki.editing.Editor]
  lazy val Links = interface[querki.links.Links]
  lazy val Time = interface[querki.time.Time]
  lazy val System = interface[querki.system.System]
  
  lazy val SystemSpace = System.State
  lazy val systemId = SystemSpace.id
  
  var _nextId = 1
  // Mapping from MySQL Primary Key to Thing OID
  case class RowKey(table:TableName, id:SQLVal[_])
  var idMap = Map.empty[RowKey, OID]
  // Mapping from MySQL Column to Property
  var colMap = Map.empty[ColumnInfo, AnyProp]
  // Keep track of the Properties by simple names, so that we know to insert
  // kickers when we need them
  var propNames = Set.empty[String]
  lazy val spaceId = createOID()
  var modelMap = Map.empty[TableName, OID]
  // Map to get to Properties by Table/Column. This is different from colMap above,
  // which uses *all* of the info about the column as a key, to decide when we can
  // merge columns in multiple tables.
  case class TableAndColumn(tbl:TableName, col:ColumnName)
  var propsByTable = Map.empty[TableAndColumn, AnyProp]
  // The Thing/Property pairs that are waiting for Links to be ready:
  case class PendingLink(tId:OID, prop:Property[OID,_], key:RowKey)
  var pendingLinks = Set.empty[PendingLink]
  
  /**
   *  Create an Import OID for a Thing that is based on a row in the MySQL. 
   *  
   *  This will then get translated to a real OID when we create the real Space.
   *  
   *  @param importedKey The primary key of the row in MySQL, if any.
   */
  def createOID():OID = {
    val oid = OID(1, _nextId)
    _nextId += 1
    oid
  }
  
  /**
   * Look at each column in this Table, and see whether it gets generated.
   */
  def checkPropGeneration(table:MySQLTable):MySQLTable = {
    // Go through the columns and see if there are any to be omitted:
    (table /: table.columns.values) { (tbl, col) =>
      def columnOmitted() = tbl.copy(columns = tbl.columns + (col.name -> col.copy(generateProp = false)))
      
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
    val tables = db.tables
    (db /: tables) { (dba, tablePair) =>
      val (name, table) = tablePair
      val withGeneration = checkPropGeneration(table)
      val withConstr = withConstraints(withGeneration)
      dba.copy(tables = dba.tables + (name -> withConstr))
    }
  }
  
  def initialState:SpaceState = {
    SpaceState(
      spaceId,
      systemId,
      () => Map(Core.NameProp(name)),
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
    words.mkString(" ")
  }
  
  /**
   * We need to make sure we choose a name for this column's Property that isn't already in use,
   * because duplicate Names are disallowed.
   */
  def choosePropName(col:MySQLColumn, tbl:MySQLTable):String = {
    val rawName = col.col.name.v
    val name = 
      if (propNames.contains(rawName) || SystemSpace.anythingByName(rawName).isDefined)
        s"${tbl.name.v} $rawName"
      else
        rawName
    propNames += name
    name
  }
  def buildProperty(col:MySQLColumn, tbl:MySQLTable):Option[AnyProp] = {
    if (!col.generateProp) {
      // We are intentionally not generating this column. This happens in cases like autoincremented IDs
      // and update timestamps.
      None
    } else {
      // TODO: detect join tables, and build a List if found. But that'll require significant and fairly
      // smart tweaking.
      val collection = 
        if (col.nullable)
          Core.Optional
        else
          Core.ExactlyOne
          
      def charHeuristic(size:Int) = {
        if (size < 128)
          Core.TextType
        else
          Core.LargeTextType
      }
      
      val constraint = tbl.constraints.find(_.localCol == col.col.name)
      def intType(size:Int) =
        if (size == 1)
          Core.YesNoType
        else
          Core.IntType
      val pType:PType[_] with PTypeBuilder[_,_] =
        if (constraint.isDefined) {
          // If it's constrained, then we presume it's a Link:
          Core.LinkType
        } else {
          // As we add more SQLTypes, make sure this keeps up:
          col.tpe match {
            case SQLInt(size:Int) => intType(size)
            case SQLUInt(size:Int) => intType(size)
            case SQLBigInt(size:Int) => Core.LongType
            case SQLDouble => Core.FloatType
            case SQLChar(size:Int) => charHeuristic(size) 
            case SQLVarchar(size:Int) => charHeuristic(size)
            case SQLLongtext => Core.LargeTextType
            case SQLDate => Time.QDateTime
            case SQLTimestamp => Time.QDateTime
            case _ => throw new Exception(s"Trying to build a Property for unknown SQLType ${col.tpe}")
          }
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
          case Some(prop) => {
            propsByTable += (TableAndColumn(table.name, col.col.name) -> prop)
            colState // Already created by another Table
          }
          case None => {
            buildProperty(col, table) match {
              case Some(prop) => {
                colMap += (col.col -> prop)
                propsByTable += (TableAndColumn(table.name, col.col.name) -> prop)
                colState.copy(spaceProps = colState.spaceProps + (prop.id -> prop))
              }
              case None => colState
            }
          }
        }
      }
    }
  }
  
  def buildQValue[RT](col:MySQLColumn, prop:Property[_,RT], sqlVal:SQLVal[_]):Option[QValue] = {
    if (sqlVal == NullVal)
      None
    else {
      val v:RT = sqlVal.v.asInstanceOf[RT]
      Some(QValue.make(prop.cType, prop.pType, v))
    }
  }
  
  /**
   * See if we can make a heuristic guess at a name to show.
   */
  def buildComputedName(table:MySQLTable):Option[(OID, QValue)] = {
    val textProps:Seq[(MySQLColumn, AnyProp)] = 
      table.columns.values.toSeq
      .map(col => (col, propsByTable.get(TableAndColumn(table.name, col.name))))
      .filter(pair => pair._2.isDefined && pair._2.get.pType == Core.TextType)
      .map(pair => (pair._1, pair._2.get))
      
    if (textProps.length == 1) {
      val (col, prop) = textProps.head
      
      def computedName() = {
        Some(Basic.ComputedNameProp(s"[[${prop.displayName}]]"))
      }
      
      // Okay, there's only one Text property. Does each row have a unique value?
      table.data match {
        case Some(MySQLData(colOrder, rows)) => {
          val index = colOrder.indexWhere { _ == col.name }
          val result = ((Set.empty[String], true) /: rows) { (pair, row) =>
            val (set, succ) = pair
            if (succ) {
              val v = row.vs(index).v.toString()
              if (set.contains(v))
                // Found a duplicate:
                (set, false)
              else
                (set + v, true)
            } else {
              // We're already failed
              pair
            }
          }
          if (result._2)
            computedName()
          else
            None
        }
        case None => {
          // There isn't any data. Okay, we'll take a stab in the dark and say that
          // this is intended to be a name column:
          computedName()
        }
      }
    } else {
      None
    }
  }
  
  def buildModels(db:MySQLDB, stateIn:SpaceState):SpaceState = {
    (stateIn /: db.tables.values) { (state, table) =>
      val mainPropPairs = table.columns.values.map { col =>
        colMap.get(col.col).map { prop =>
          val default:QValue = col.defaultOpt.filter(_ != NullVal).flatMap(v => buildQValue(col, prop, v)).getOrElse(prop.default(stateIn))
          (prop.id, default)
        }
      }.flatten.toSeq 
      val propPairs = mainPropPairs ++ Seq(
        Core.IsModelProp(true),
        // Note that the Instance Props intentionally doesn't include Display Name. Until we have an interactive
        // way for the user to specify a Display Name field, we use Computed Name instead.
        Editor.InstanceProps(mainPropPairs.map(_._1):_*),
        Basic.DisplayNameProp(fixName(table.name.v))
      ) ++ buildComputedName(table)
      
      val oid = createOID()
      modelMap += (table.name -> oid)
      
      val model = ThingState(
        oid,
        spaceId,
        Basic.SimpleThing.id,
        () => Map(propPairs:_*)
      )
      
      state.copy(things = state.things + (oid -> model))
    }
  }
  
  /**
   * Use the MySQL Constraints to build Querki Link Properties.
   */
  def addConstraints(db:MySQLDB, stateIn:SpaceState):SpaceState = {
    (stateIn /: db.tables.values) { (tblState, table) =>
      (tblState /: table.constraints) { (constState, constraint) =>
        val model = constState.anything(modelMap(table.name)).get.asInstanceOf[ThingState]
        val localCol = table.columns(constraint.localCol)
        val localProp = colMap(localCol.col)
        val foreignTbl = db.tables(constraint.foreignTable)
        val foreignCol = foreignTbl.columns(constraint.foreignCol)
        if (!foreignTbl.primaryKey.isDefined || !(foreignTbl.primaryKey.get == foreignCol.name)) {
          throw new Exception(s"We can currently only deal with Constraints to primary keys! Failed constraint ${table.name}.${foreignCol.name}")
        }
        val prop = colMap(localCol.col)
        if (prop.pType != Core.LinkType) {
          throw new Exception(s"Somehow wound up with constraint $constraint not pointing to a Link Property! Col = $localCol; prop = $prop (${prop.cType} of ${prop.pType})")
        }
        // Okay -- we have a valid Constraint. Fill in the Link Model:
        val linkedModelId = modelMap(foreignTbl.name)
        val linkPair = Links.LinkModelProp(linkedModelId)
        val fixedName = {
          val propName = prop.linkName.get
          if (propName.endsWith(" Id")) {
            val candidate = propName.substring(0, propName.length - 3)
            constState.anythingByName(candidate) match {
              case Some(_) => propName   // Nope, it's taken
              case None => candidate
            }
          } else
            propName
        }
        val tweakedProp = prop.copy(pf = () => prop.props + linkPair + Core.setName(fixedName))
        constState.copy(spaceProps = constState.spaceProps + (prop.id -> tweakedProp))
      }
    }
  }
  
  /**
   * Build up this Instance from the given parallel lists, recursively.
   * 
   * Note that we explicitly assume that all three lists are the same length.
   */
  @tailrec
  private def buildInstance(db:MySQLDB, table:MySQLTable, primaryOpt:Option[ColumnName], tIn:ThingState, cols:Seq[MySQLColumn], props:Seq[Option[AnyProp]], vals:Seq[SQLVal[_]]):ThingState = {
    if (cols.isEmpty)
      tIn
    else {
      val col = cols.head
      val v = vals.head
      // First, record the ID cross-reference, if this is the primary key. We need to do this regardless
      // of whether we're actually emitting the Property or not:
      primaryOpt.foreach { primary =>
        if (col.name == primary) {
          // This is the primary key for this row, so add it to the mapping, so that we can use
          // it for links later if needed:
          idMap += (RowKey(table.name, v) -> tIn.id)
        }
      }
      
      props.head match {
        case Some(prop) => {
          // Usual case: generate a value for this Property, if there is one:
          val t = prop.confirmType(Core.LinkType) match {
            // We don't want to deal with Links until all the Things are created:
            case Some(linkProp) => {
              val foreignTable = col.rawConstraint.map(constr => db.tables(constr.foreignTable).name).get
              pendingLinks += (PendingLink(tIn.id, linkProp, RowKey(foreignTable, v)))
              tIn
            }
            case _ if (col.generateProp) => {
              // Ordinary column -- add this row's value to the Thing:
              buildQValue(col, prop, v) match {
                case Some(qv) => tIn.copy(pf = () => tIn.props + (prop.id -> qv))
                case None => tIn
              }
            }
            case _ => tIn  // This Column doesn't generate a Property
          }
          buildInstance(db, table, primaryOpt, t, cols.tail, props.tail, vals.tail)          
        }
        case None => {
          // There's no Property, which means we're omitting this column. Just keep going:
          buildInstance(db, table, primaryOpt, tIn, cols.tail, props.tail, vals.tail)
        }
      }
    }
  }
  def buildInstances(db:MySQLDB, stateIn:SpaceState):SpaceState = {
    (stateIn /: db.tables.values) { (tblState, table) =>
      val primary = table.primaryKey
      table.data match {
        case Some(data) => {
          val props = data.columnOrder.map(colName => propsByTable.get(TableAndColumn(table.name, colName)))
          val cols = data.columnOrder.map(table.columns(_))
          // There are rows in this table, which need to be turned into Things
          (tblState /: data.rows) { (rowState, row) =>
            val tInit = 
              ThingState(
                createOID(),
                spaceId,
                modelMap(table.name),
                () => emptyProps
              )
            val t = buildInstance(db, table, primary, tInit, cols, props, row.vs)
            rowState.copy(things = rowState.things + (t.id -> t))
          }
        }
        case None => tblState // No INSERTs for this table
      }
    }
  }
  
  def addLinks(db:MySQLDB, stateIn:SpaceState):SpaceState = {
    (stateIn /: pendingLinks.iterator) { (state, pending) =>
      val PendingLink(tId:OID, prop:Property[OID,Any], key @ RowKey(tableName, v)) = pending
      val tIn = state.thing(tId)
      val t =
        if (v == NullVal) {
          // Nulls are only legal for Optional Properties:
          if (prop.cType != Core.Optional)
            throw new Exception(s"Somehow got a NULL value for non-nullable Property $prop on Thing $tIn")
          // Just leave it empty
          // TODO: there's an implicit assumption here that Links default to NULL. That might be wrong.
          tIn
        } else {
          val linkId = idMap(key)
          tIn.copy(pf = () => tIn.props + prop(linkId))
        }
      state.copy(things = state.things + (t.id -> t))
    }
  }
  
  def buildSpaceState(initDB:MySQLDB):SpaceState = {
    val db = preprocessDB(initDB)
    
    val initState = initialState
    val withProps = buildProperties(db, initState)
    val withModels = buildModels(db, withProps)
    val withConstraints = addConstraints(db, withModels)
    val withInstances = buildInstances(db, withConstraints)
    val withLinks = addLinks(db, withInstances)
    
    withLinks
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

