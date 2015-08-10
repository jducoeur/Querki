package querki.imexport

import fastparse.all._

import querki.globals._
import querki.values.RequestContext

object MySQLParse {
  sealed trait Stmt
  case class StmtDrop(name:String) extends Stmt
  case class StmtInsert(table:TableName, cols:Seq[ColumnName], rows:Seq[RawRow]) extends Stmt
  case class StmtLock(name:String) extends Stmt
  case object StmtUnlock extends Stmt
  
  // To keep thing type-checked, use some value classes:
  case class ColumnName(val v:String) extends AnyVal
  case class TableName(val v:String) extends AnyVal
  case class RawRow(val v:Seq[String]) extends AnyVal
  
  val nlP = P("\r".? ~ "\n")
  val whiteP = P(CharsWhile(_.isWhitespace))
  
  // The various sorts of comments you can find in the dump. None of these product anything.
  // Note that, for the time being at least, we ignore the MySQL pragmas inside delim comments.
  val hashCommentP = P("# " ~ (!nlP ~ AnyChar).rep ~ nlP)
  val dashCommentP = P("-- " ~ (!nlP ~ AnyChar).rep ~ nlP)
  val delimCommentP = P("/*" ~ AnyChar.rep ~ "*/")
  val commentP = P(hashCommentP | dashCommentP | delimCommentP)
  val commentsP = P(commentP.rep)
  
  val identP = P(CharsWhile(c => CharPredicates.isLetter(c) || CharPredicates.isDigit(c) || c == '_' || c == '-').!)
  val quotedIdentP = P("`" ~ identP.! ~ "`")
  
  val dropStatementP = P("DROP TABLE IF EXISTS `" ~ identP ~ "`").map { ident => StmtDrop(ident) }
  val columnsClauseP = P("(" ~ quotedIdentP.rep(1, sep=", ") ~ ")") map { _.map(ColumnName(_)) }
  val quotedValueP = P("`" ~ AnyChar.rep.! ~ "`")
  val oneValueP = P(quotedValueP | (!"," ~ AnyChar).rep.!)
  val rowValuesP = P("(" ~ oneValueP.!.rep(sep=",") ~ ")").map(RawRow(_))
  val insertStatementP = P("INSERT INTO " ~ quotedIdentP ~ whiteP ~ columnsClauseP ~ whiteP ~ 
      "VALUES" ~ whiteP ~ rowValuesP.rep(sep="," ~ whiteP)) map { content =>
        val (tblName, colNames, rows) = content
        StmtInsert(TableName(tblName), colNames, rows)
      }
  val lockStatementP = P("LOCK TABLES `" ~ identP ~ "` WRITE").map { ident => StmtLock(ident) }
  val unlockStatementP = P("UNLOCK TABLES").map { dummy => StmtUnlock }
  
  val statementContentP:Parser[Stmt] = P(dropStatementP | insertStatementP | lockStatementP | unlockStatementP)
  val statementP = P(statementContentP ~ ";")
  
  val dumpfileP = P(commentsP ~ statementP.rep(sep=commentsP) ~ commentsP)
  
  def apply(mySQL:String):Seq[Stmt] = {
    dumpfileP.parse(mySQL).get.value
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

  def readDumpfile(mySQL:String):SpaceState = {
    val statements = MySQLParse(mySQL)
    
    ???
  }
}

object MySQLImport {
  import MySQLParse._
  
  sealed trait MySQLType
  case object SQLInt
  case object SQLUInt
  case object SQLBigInt
  case object SQLAutoIncrement
  case object SQLDouble
  case object SQLChar
  case object SQLVarchar
  case object SQLLongtext
  case object SQLDate
  case object SQLTimestamp
  
  case class MySQLDB(tables:Map[TableName, MySQLTable])
  case class MySQLTable(name:TableName, columns:Map[ColumnName, MySQLColumn], primaryKey:ColumnName)
  case class MySQLColumn(name:ColumnName, nullable:Boolean, default:Option[String])
  case class MySQLConstraint(localColumn:ColumnName, foreignTable:TableName, foreignColumn:ColumnName)
  case class MySQLRawData(table:MySQLTable, columnOrder:Seq[ColumnName], rows:Seq[RawRow])
}
