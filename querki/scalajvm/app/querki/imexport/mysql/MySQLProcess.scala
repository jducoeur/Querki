package querki.imexport.mysql

import querki.time._

/**
 * The second phase of importing MySQL files -- take the raw statements, and build
 * a "database" from them.
 */
object MySQLProcess {
  import MySQLParse._
  import MySQLProcess._
  
  case class MySQLDB(tables:Map[TableName, MySQLTable])
  case class MySQLTable(name:TableName, columns:Map[ColumnName, MySQLColumn], primaryKey:Option[ColumnName], 
      constraints:Seq[SQLConstraint], data:Option[MySQLData] = None)
  case class MySQLRow(vs:Seq[SQLVal])
  case class MySQLColumn(col:ColumnInfo, 
      generateProp:Boolean = true, rawConstraint:Option[SQLConstraint] = None) 
  {
    def clauses = col.clauses
    def name = col.name
    def tpe = col.tpe
    lazy val nullable = !col.clauses.contains(SQLNotNull)
  }
  case class MySQLData(columnOrder:Seq[ColumnName], rows:Seq[MySQLRow])
  
  sealed trait SQLVal
  case object NullVal extends SQLVal
  case class BoolVal(v:Boolean) extends SQLVal
  case class IntVal(v:Int) extends SQLVal
  case class UIntVal(v:Int) extends SQLVal
  case class BigIntVal(v:Long) extends SQLVal
  case class DoubleVal(v:Double) extends SQLVal
  case class CharVal(v:String) extends SQLVal
  case class VarcharVal(v:String) extends SQLVal
  case class LongtextVal(v:String) extends SQLVal
  case class DateVal(v:DateTime) extends SQLVal
  case class TimestampVal(v:DateTime) extends SQLVal
  
  def processCreate(stmt:StmtCreate, db:MySQLDB):MySQLDB = {
    val StmtCreate(name, cols, xrefs) = stmt
    val colMap = Map(cols.map(MySQLColumn(_)).map(col => col.name -> col):_*)
    val primary = xrefs.collectFirst { case SQLPrimaryKey(col) => col }
    val constraints = xrefs.collect { case con:SQLConstraint => con }
    val table = MySQLTable(name, colMap, primary, constraints)
    
    db.copy(tables = db.tables + (name -> table))
  }
  
  val dateFormat = DateTimeFormat.forPattern("yyyy-MM-dd")
  val timestampFormat = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")
  
  def processVal(col:MySQLColumn, v:String):SQLVal = {
    def parseInt(size:Int) = {
      if (size == 1)
        BoolVal(v == "1")
      else
        IntVal(v.toInt)
    }
    
    if (col.nullable && v == "NULL")
      NullVal
    else col.tpe match {
      case SQLInt(size:Int) => parseInt(size)
      case SQLUInt(size:Int) => parseInt(size)
      case SQLBigInt(size:Int) => BigIntVal(v.toLong)
      case SQLDouble => DoubleVal(v.toDouble)
      case SQLChar(size:Int) => CharVal(v)
      case SQLVarchar(size:Int) => VarcharVal(v)
      case SQLLongtext => LongtextVal(v)
      case SQLDate => DateVal(dateFormat.parseDateTime(v)) 
      case SQLTimestamp => TimestampVal(timestampFormat.parseDateTime(v))
    }
  }
  
  def processRow(cols:Seq[MySQLColumn], row:RawRow):MySQLRow = {
    MySQLRow(cols.zip(row.v).map(pair => processVal(pair._1, pair._2)))
  }
  
  def processInsert(stmt:StmtInsert, db:MySQLDB):MySQLDB = {
    val StmtInsert(tableName:TableName, colNames:Seq[ColumnName], rawRows:Seq[RawRow]) = stmt
    val table = db.tables(tableName)
    val cols = colNames.map(colName => table.columns(colName))
    val rows = rawRows.map(processRow(cols, _))
    val tableWithData = table.copy(data = Some(MySQLData(colNames, rows)))
    db.copy(tables = db.tables + (tableName -> tableWithData))
  }
  
  def processStmt(db:MySQLDB, stmt:Stmt):MySQLDB = {
    stmt match {
      case create:StmtCreate => processCreate(create, db)
      
      case StmtDrop(TableName(name)) => db.copy(tables = db.tables - TableName(name))
      
      case insert:StmtInsert => processInsert(insert, db)
      
      case StmtLock(TableName(name)) => db
        
      case StmtUnlock => db
    }
  }
  def processStmts(statements:Seq[Stmt]):MySQLDB = {
    (MySQLDB(Map.empty) /: statements) { (db, stmt) =>
      processStmt(db, stmt)
    }    
  }
}