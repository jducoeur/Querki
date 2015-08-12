package querki.imexport

import fastparse.all._
import fastparse.core.Result._

import querki.globals._
import querki.time._
import querki.values.RequestContext

object MySQLParse {
  sealed trait Stmt
  case class StmtCreate(name:TableName, cols:Seq[ColumnInfo], xrefs:Seq[SQLXref]) extends Stmt
  case class StmtDrop(name:TableName) extends Stmt
  case class StmtInsert(table:TableName, cols:Seq[ColumnName], rows:Seq[RawRow]) extends Stmt
  case class StmtLock(name:TableName) extends Stmt
  case object StmtUnlock extends Stmt
  
  // To keep thing type-checked, use some value classes:
  case class ColumnName(val v:String) extends AnyVal
  case class TableName(val v:String) extends AnyVal
  case class RawRow(val v:Seq[String]) extends AnyVal
  
  sealed trait SQLType
  case class SQLInt(size:Int) extends SQLType
  case class SQLUInt(size:Int) extends SQLType
  case class SQLBigInt(size:Int) extends SQLType
  case object SQLDouble extends SQLType
  case class SQLChar(size:Int) extends SQLType
  case class SQLVarchar(size:Int) extends SQLType
  case object SQLLongtext extends SQLType
  case object SQLDate extends SQLType
  case object SQLTimestamp extends SQLType
  
  sealed trait SQLColumnOpt
  case object SQLAutoIncrement extends SQLColumnOpt
  case object SQLNull extends SQLColumnOpt
  case object SQLNotNull extends SQLColumnOpt
  case class SQLDefault(v:String) extends SQLColumnOpt
  case class SQLOnUpdate(v:SQLUpdateOpt) extends SQLColumnOpt
  
  sealed trait SQLUpdateOpt extends SQLColumnOpt
  case object SQLCurrentTimestamp extends SQLUpdateOpt
  case object SQLSetNull extends SQLUpdateOpt
  case object SQLCascade extends SQLUpdateOpt
  
  sealed trait SQLXref
  case class SQLPrimaryKey(col:ColumnName) extends SQLXref
  // We're not worrying about keys, at least for now
  case object SQLKey extends SQLXref
  case class SQLConstraint(localCol:ColumnName, foreignTable:TableName, foreignCol:ColumnName, 
      update:Option[SQLUpdateOpt], delete:Option[SQLUpdateOpt]) extends SQLXref
  
  case class ColumnInfo(name:ColumnName, tpe:SQLType, clauses:Seq[SQLColumnOpt])
  
  val nlP = P("\r".? ~ "\n")
  val wP = P(CharsWhile(_.isWhitespace))
  val wOptP = P(CharsWhile(_.isWhitespace, 0))
  
  // The various sorts of comments you can find in the dump. None of these product anything.
  // Note that, for the time being at least, we ignore the MySQL pragmas inside delim comments.
  val blankCommentP = P(CharsWhile(c => c != '\n' && c != 'r' && c.isWhitespace, 0) ~ nlP)
  val hashCommentP = P("#" ~! (!nlP ~ AnyChar).rep ~ nlP)
  val dashCommentP = P("-- " ~! (!nlP ~ AnyChar).rep ~ nlP)
  val delimCommentP = P("/*" ~! (!"*/" ~ AnyChar).rep ~ "*/" ~ ";".?)
  val commentP = P(blankCommentP | hashCommentP | dashCommentP | delimCommentP)
  val commentsP = P(commentP.rep)
  
  val identP = P(CharsWhile(c => CharPredicates.isLetter(c) || CharPredicates.isDigit(c) || c == '_' || c == '-').!)
  val quotedIdentP = P("`" ~ identP.! ~ "`")
  
  val sizeP = P("(" ~ CharsWhile(_.isDigit).! ~ ")") map { str => Integer.parseInt(str) }
  val uintP = P("int" ~ sizeP ~ wP ~ "unsigned") map { SQLUInt(_) }
  val intP = P("int" ~ sizeP) map { SQLInt(_) }
  val bigintP = P("bigint" ~ sizeP) map { SQLBigInt(_) }
  val doubleP = P("double") map { dummy => SQLDouble }
  val charP = P("char" ~ sizeP) map { SQLChar(_) }
  val varcharP = P("varchar" ~ sizeP) map { SQLVarchar(_) }
  val longtextP = P("longtext") map { dummy => SQLLongtext }
  val dateP = P("date") map { dummy => SQLDate }
  val timestampP = P("timestamp") map { dummy => SQLTimestamp }
  val typeDefP:Parser[SQLType] = P(uintP | intP | bigintP | doubleP | charP | varcharP | longtextP | dateP | timestampP)
  
  val autoIncrementP = P("AUTO_INCREMENT") map { dummy => SQLAutoIncrement }
  val nullP = P("NULL") map { dummy => SQLNull }
  val notNullP = P("NOT NULL") map { dummy => SQLNotNull }
  val defaultP = P("DEFAULT" ~ wP ~ oneValueP) map { SQLDefault(_) }
  val curTimestampP = P("CURRENT_TIMESTAMP") map { dummy => SQLCurrentTimestamp }
  val setNullP = P("SET NULL") map { dummy => SQLSetNull }
  val cascadeP = P("CASCADE") map { dummy => SQLCascade }
  val updateOptP:Parser[SQLUpdateOpt] = P(curTimestampP | setNullP | cascadeP)
  val onUpdateP = P("ON UPDATE" ~! wP ~ updateOptP)
  val onDeleteP = P("ON DELETE" ~! wP ~ updateOptP)
  val columnOptP:Parser[SQLColumnOpt] = P(autoIncrementP | nullP | notNullP | defaultP | updateOptP)
  
  val primaryP = P("PRIMARY KEY (" ~ quotedIdentP ~ ")") map { ident => SQLPrimaryKey(ColumnName(ident)) }
  val keyP = P("KEY " ~ quotedIdentP ~ " (" ~ quotedIdentP ~ ")") map { idents => SQLKey }
  val constraintP = P("CONSTRAINT " ~ quotedIdentP ~ " FOREIGN KEY (" ~ quotedIdentP ~ 
      ") REFERENCES " ~ quotedIdentP ~ " (" ~ quotedIdentP ~ ")" ~ wOptP ~ onDeleteP.? ~ wOptP ~ onUpdateP.?) map
      { constr =>
        val (dummy, localColumn, foreignTable, foreignColumn, onDelete, onUpdate) = constr
        SQLConstraint(ColumnName(localColumn), TableName(foreignTable), ColumnName(foreignColumn), onDelete, onUpdate)
      }
  val xrefP:Parser[SQLXref] = P(primaryP | keyP | constraintP)
  
  // Note that, for the moment, we're just ignoring the tableOpts:
  val engineP = P("ENGINE=" ~ ("InnoDB" | "MyISAM"))
  val charsetP = P("DEFAULT CHARSET=" ~ ("utf8" | "latin1"))
  val tableOptsP = P((engineP | charsetP).rep(sep=wP))
  
  val columnDefP = P(quotedIdentP ~ wP ~! typeDefP ~ wOptP ~ columnOptP.rep(sep = wP)) map
    { info =>
      val (name, tpe, opts) = info
      ColumnInfo(ColumnName(name), tpe, opts)
    }
  val createStatementP = P("CREATE TABLE " ~ quotedIdentP ~! wP ~ "(" ~! wP ~ columnDefP.rep(sep = "," ~ wP) 
      ~ ("," ~ wP ~ xrefP.rep(sep="," ~! wP)).? ~ wP ~ ")" ~ wOptP ~ tableOptsP) map
      { info =>
        val (name, cols, xrefs) = info
        StmtCreate(TableName(name), cols, xrefs.getOrElse(Seq.empty))
      }
  
  val dropStatementP = P("DROP TABLE IF EXISTS `" ~ identP ~ "`").map { ident => StmtDrop(TableName(ident)) }
  
  val columnsClauseP = P("(" ~ quotedIdentP.rep(1, sep=", ") ~ ")") map { _.map(ColumnName(_)) }
  val quotedValueP = P("'" ~ ("\\'" | (!"'" ~ AnyChar)).rep.! ~ "'")
  val oneValueP = P(quotedValueP | (!("," | ")") ~ AnyChar).rep.!)
  val rowValuesP = P("(" ~ oneValueP.!.rep(sep="," ~! Pass) ~! ")").map(RawRow(_))
  val insertStatementP = P("INSERT INTO " ~ quotedIdentP ~ wP ~ columnsClauseP ~ wP ~ 
      "VALUES" ~ wP ~ rowValuesP.rep(sep="," ~ wP)) map { content =>
        val (tblName, colNames, rows) = content
        StmtInsert(TableName(tblName), colNames, rows)
      }
  
  val lockStatementP = P("LOCK TABLES `" ~ identP ~ "` WRITE").map { ident => StmtLock(TableName(ident)) }
  
  val unlockStatementP = P("UNLOCK TABLES").map { dummy => StmtUnlock }
  
  val statementContentP:Parser[Stmt] = P(createStatementP | dropStatementP | insertStatementP | lockStatementP | unlockStatementP)
  val statementP = P(statementContentP ~ ";" ~! nlP.?)
  
  val dumpfileP = P(Start ~ commentsP ~! statementP.rep(sep=commentsP) ~ commentsP ~ End)
  
  def apply(mySQL:String):Seq[Stmt] = {
    dumpfileP.parse(mySQL) match {
      case Success(stmts, _) => stmts
      case Failure(parser, index) => {
        val start = 
          if (index < 10)
            index
          else
            index - 10
        throw new Exception(s"Attempt to parse MySQL failed in $parser at $index:\n...${mySQL.slice(start, index)}[${mySQL.slice(index, index + 20)}]...")
      }
    }
  }
}

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
  import MySQLImport._
  
  def processCreate(stmt:StmtCreate, db:MySQLDB):MySQLDB = {
    val StmtCreate(name, cols, xrefs) = stmt
    val colMap = Map(cols.map(col => col.name -> col):_*)
    val primary = xrefs.collectFirst { case SQLPrimaryKey(col) => col }
    val constraints = xrefs.collect { case con:SQLConstraint => con }
    val table = MySQLTable(name, colMap, primary, constraints)
    
    db.copy(tables = db.tables + (name -> table))
  }
  
  val dateFormat = DateTimeFormat.forPattern("yyyy-MM-dd")
  val timestampFormat = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")
  
  def processVal(col:ColumnInfo, v:String):SQLVal = {
    def parseInt(size:Int) = {
      if (size == 1)
        BoolVal(v == "1")
      else
        IntVal(v.toInt)
    }
    
    col.tpe match {
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
  
  def processRow(cols:Seq[ColumnInfo], row:RawRow):MySQLRow = {
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

  def readDumpfile(mySQL:String):SpaceState = {
    // First, do the raw parse of the file, which results in a series of statements:
    val statements = MySQLParse(mySQL)
    
    // Now, go through the statements in order, and build a MySQLDB out of them:
    val db = processStmts(statements)
    
    ???
  }
}

object MySQLImport {
  import MySQLParse._
  
  case class MySQLDB(tables:Map[TableName, MySQLTable])
  case class MySQLTable(name:TableName, columns:Map[ColumnName, ColumnInfo], primaryKey:Option[ColumnName], 
      constraints:Seq[SQLConstraint], data:Option[MySQLData] = None)
  case class MySQLRow(vs:Seq[SQLVal])
  case class MySQLData(columnOrder:Seq[ColumnName], rows:Seq[MySQLRow])
  
  sealed trait SQLVal
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
}
