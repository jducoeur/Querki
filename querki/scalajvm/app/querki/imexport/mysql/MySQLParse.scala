package querki.imexport.mysql

import fastparse._, NoWhitespace._

import querki.globals._

object MySQLParse {
  sealed trait Stmt

  case class StmtCreate(
    name: TableName,
    cols: Seq[ColumnInfo],
    xrefs: Seq[SQLXref]
  ) extends Stmt
  case class StmtDrop(name: TableName) extends Stmt

  case class StmtInsert(
    table: TableName,
    cols: Option[Seq[ColumnName]],
    rows: Seq[RawRow]
  ) extends Stmt
  case class StmtLock(name: TableName) extends Stmt
  case object StmtUnlock extends Stmt

  // To keep thing type-checked, use some value classes:
  case class ColumnName(val v: String) extends AnyVal
  case class TableName(val v: String) extends AnyVal
  case class RawRow(val v: Seq[String]) extends AnyVal

  sealed trait SQLType
  case class SQLInt(size: Int) extends SQLType
  case class SQLUInt(size: Int) extends SQLType
  case class SQLBigInt(size: Int) extends SQLType
  case object SQLDouble extends SQLType
  case class SQLChar(size: Int) extends SQLType
  case class SQLVarchar(size: Int) extends SQLType
  case object SQLLongtext extends SQLType
  case object SQLDate extends SQLType
  case object SQLTimestamp extends SQLType

  sealed trait SQLColumnOpt
  case object SQLAutoIncrement extends SQLColumnOpt
  case object SQLNull extends SQLColumnOpt
  case object SQLNotNull extends SQLColumnOpt
  case class SQLDefault(v: String) extends SQLColumnOpt
  case class SQLOnUpdate(v: SQLUpdateOpt) extends SQLColumnOpt
  case class SQLOnDelete(v: SQLUpdateOpt) extends SQLColumnOpt

  sealed trait SQLUpdateOpt
  case object SQLCurrentTimestamp extends SQLUpdateOpt
  case object SQLSetNull extends SQLUpdateOpt
  case object SQLCascade extends SQLUpdateOpt

  sealed trait SQLXref
  case class SQLPrimaryKey(col: ColumnName) extends SQLXref
  // We're not worrying about keys, at least for now
  case object SQLKey extends SQLXref

  case class SQLConstraint(
    localCol: ColumnName,
    foreignTable: TableName,
    foreignCol: ColumnName,
    update: Option[SQLOnUpdate],
    delete: Option[SQLOnDelete]
  ) extends SQLXref

  case class ColumnInfo(
    name: ColumnName,
    tpe: SQLType,
    clauses: Seq[SQLColumnOpt]
  )

  def nlP[_ : P] = P("\r".? ~ "\n")
  def wP[_ : P] = P(CharsWhile(_.isWhitespace))
  def wOptP[_ : P] = P(CharsWhile(_.isWhitespace, 0))

  // The various sorts of comments you can find in the dump. None of these product anything.
  // Note that, for the time being at least, we ignore the MySQL pragmas inside delim comments.
  def blankCommentP[_ : P] = P(CharsWhile(c => c != '\n' && c != 'r' && c.isWhitespace, 0) ~ nlP)
  def hashCommentP[_ : P] = P("#" ~ (!nlP ~ AnyChar).rep ~ nlP)
  def dashCommentP[_ : P] = P("--" ~ (!nlP ~ AnyChar).rep ~ nlP)
  def delimCommentP[_ : P] = P("/*" ~ (!"*/" ~ AnyChar).rep ~ "*/" ~ ";".?)
  def commentP[_ : P] = P(blankCommentP | hashCommentP | dashCommentP | delimCommentP)
  def commentsP[_ : P] = P(commentP.rep)

  def identP[_ : P] =
    P(CharsWhile(c => CharPredicates.isLetter(c) || CharPredicates.isDigit(c) || c == '_' || c == '-').!)
  def quotedIdentP[_ : P] = P("`" ~ identP.! ~ "`")

  def sizeP[_ : P] = P("(" ~ CharsWhile(_.isDigit).! ~ ")").map { str => Integer.parseInt(str) }
  def uintP[_ : P] = P("int" ~ sizeP ~ wP ~ "unsigned").map { SQLUInt(_) }
  def intP[_ : P] = P("int" ~ sizeP).map { SQLInt(_) }
  def bigintP[_ : P] = P("bigint" ~ sizeP).map { SQLBigInt(_) }
  def doubleP[_ : P] = P("double").map { dummy => SQLDouble }
  def charP[_ : P] = P("char" ~ sizeP).map { SQLChar(_) }
  def varcharP[_ : P] = P("varchar" ~ sizeP).map { SQLVarchar(_) }
  def longtextP[_ : P] = P("longtext").map { dummy => SQLLongtext }
  def dateP[_ : P] = P("date").map { dummy => SQLDate }
  def timestampP[_ : P] = P("timestamp").map { dummy => SQLTimestamp }

  def typeDefP[_ : P]: P[SQLType] =
    P(uintP | intP | bigintP | doubleP | charP | varcharP | longtextP | dateP | timestampP)

  def autoIncrementP[_ : P] = P("AUTO_INCREMENT").map { dummy => SQLAutoIncrement }
  def nullP[_ : P] = P("NULL").map { dummy => SQLNull }
  def notNullP[_ : P] = P("NOT NULL").map { dummy => SQLNotNull }
  def defaultP[_ : P] = P("DEFAULT" ~ wP ~ oneValueP).map { SQLDefault(_) }
  def curTimestampP[_ : P] = P("CURRENT_TIMESTAMP").map { dummy => SQLCurrentTimestamp }
  def setNullP[_ : P] = P("SET NULL").map { dummy => SQLSetNull }
  def cascadeP[_ : P] = P("CASCADE").map { dummy => SQLCascade }
  def updateOptP[_ : P]: P[SQLUpdateOpt] = P(curTimestampP | setNullP | cascadeP)
  def onUpdateP[_ : P] = P("ON UPDATE" ~/ wP ~ updateOptP).map { SQLOnUpdate(_) }
  def onDeleteP[_ : P] = P("ON DELETE" ~/ wP ~ updateOptP).map { SQLOnDelete(_) }
  def columnOptP[_ : P]: P[SQLColumnOpt] = P(autoIncrementP | nullP | notNullP | defaultP | onUpdateP | onDeleteP)

  def primaryP[_ : P] = P("PRIMARY KEY (" ~ quotedIdentP ~ ")").map { ident => SQLPrimaryKey(ColumnName(ident)) }
  def keyP[_ : P] = P("KEY " ~ quotedIdentP ~ " (" ~ quotedIdentP ~ ")").map { idents => SQLKey }

  def constraintP[_ : P] = P("CONSTRAINT " ~ quotedIdentP ~ " FOREIGN KEY (" ~ quotedIdentP ~
    ") REFERENCES " ~ quotedIdentP ~ " (" ~ quotedIdentP ~ ")" ~ wOptP ~ onDeleteP.? ~ wOptP ~ onUpdateP.?).map {
    constr =>
      val (dummy, localColumn, foreignTable, foreignColumn, onDelete, onUpdate) = constr
      SQLConstraint(ColumnName(localColumn), TableName(foreignTable), ColumnName(foreignColumn), onUpdate, onDelete)
  }
  def xrefP[_ : P]: P[SQLXref] = P(primaryP | keyP | constraintP)

  // Note that, for the moment, we're just ignoring the tableOpts:
  def engineP[_ : P] = P("ENGINE=" ~ ("InnoDB" | "MyISAM"))
  def charsetP[_ : P] = P("DEFAULT CHARSET=" ~ ("utf8" | "latin1"))
  def currentIncrementP[_ : P] = P("AUTO_INCREMENT=" ~ CharsWhile(_.isDigit))
  def tableOptsP[_ : P] = P((engineP | charsetP | currentIncrementP).rep(sep = wP))

  def columnDefP[_ : P] = P(quotedIdentP ~ wP ~/ typeDefP ~ wOptP ~ columnOptP.rep(sep = wP)).map {
    info =>
      val (name, tpe, opts) = info
      ColumnInfo(ColumnName(name), tpe, opts)
  }

  def createStatementP[_ : P] = P("CREATE TABLE " ~ quotedIdentP ~/ wP ~ "(" ~/ wP ~ columnDefP.rep(sep = "," ~ wP)
    ~ ("," ~ wP ~ xrefP.rep(sep = "," ~/ wP)).? ~ wOptP ~ ")" ~ wOptP ~ tableOptsP).map {
    info =>
      val (name, cols, xrefs) = info
      StmtCreate(TableName(name), cols, xrefs.getOrElse(Seq.empty))
  }

  def dropStatementP[_ : P] = P("DROP TABLE IF EXISTS `" ~ identP ~ "`").map { ident => StmtDrop(TableName(ident)) }

  def columnsClauseP[_ : P] = P("(" ~ quotedIdentP.rep(1, sep = ", ") ~ ")").map { _.map(ColumnName(_)) }
  def quotedContentP[_ : P] = P(("\\'" | (!"'" ~ AnyChar)).rep.!).map { content => content.replace("\\'", "'") }
  def quotedValueP[_ : P] = P("'" ~/ quotedContentP ~ "'")
  def oneValueP[_ : P] = P(quotedValueP | "NULL".! | (!("," | ")") ~ AnyChar).rep.!)
  def rowValuesP[_ : P] = P("(" ~ oneValueP.rep(sep = "," ~/ Pass) ~/ ")").map(RawRow(_))

  def insertStatementP[_ : P] = P("INSERT INTO " ~ quotedIdentP ~ wP ~ columnsClauseP.? ~ wOptP ~
    "VALUES" ~ wP ~ rowValuesP.rep(sep = "," ~ wOptP)).map { content =>
    val (tblName, colNames, rows) = content
    StmtInsert(TableName(tblName), colNames, rows)
  }

  def lockStatementP[_ : P] = P("LOCK TABLES `" ~ identP ~ "` WRITE").map { ident => StmtLock(TableName(ident)) }

  def unlockStatementP[_ : P] = P("UNLOCK TABLES").map { dummy => StmtUnlock }

  def statementContentP[_ : P]: P[Stmt] =
    P(createStatementP | dropStatementP | insertStatementP | lockStatementP | unlockStatementP)
  def statementP[_ : P] = P(statementContentP ~ ";" ~/ nlP.?)

  def dumpfileP[_ : P] = P(Start ~ commentsP ~/ statementP.rep(sep = commentsP) ~ commentsP ~ End)

  def apply(mySQL: String): Seq[Stmt] = {
    parse(mySQL, dumpfileP(_)).fold(
      { (parser, index, extra) =>
        val start =
          if (index < 10)
            index
          else
            index - 10
        throw new Exception(
          s"Attempt to parse MySQL failed in $parser at $index:\n...${mySQL.slice(start, index)}[${mySQL.slice(index, index + 20)}]..."
        )
      },
      { (stmts, index) =>
        stmts
      }
    )
  }
}
