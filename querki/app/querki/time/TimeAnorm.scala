package querki.time

import anorm._

object TimeAnorm {
  
  /**
   * Anorm extension to convert a SQL Timestamp to a Joda DateTime.
   * 
   * Simplified from: http://stackoverflow.com/questions/11388301/joda-datetime-field-on-play-framework-2-0s-anorm/11975107#11975107
   * See that post for how to convert other SQL types, but we may not bother, so I'm keeping it simple for now.
   */
  implicit def rowToDateTime: Column[DateTime] = Column.nonNull { (value, meta) =>
    val MetaDataItem(qualified, nullable, clazz) = meta
    value match {
      case ts: java.sql.Timestamp => Right(new DateTime(ts.getTime))
      //case d: java.sql.Date => Right(new DateTime(d.getTime))
      case _ => Left(TypeDoesNotMatch("Cannot convert " + value + ":" + value.asInstanceOf[AnyRef].getClass) )
    }
  }
  
  implicit class DateTimeSqlRow(row:SqlRow) {
    def dateTime(name:String) = row.get[DateTime](name).get
  }
  
  implicit val dateTimeToStatement = new ToStatement[DateTime] {
    def set(s: java.sql.PreparedStatement, index: Int, aValue: DateTime): Unit = {
      s.setTimestamp(index, new java.sql.Timestamp(aValue.withMillisOfSecond(0).getMillis()) )
    }
  }
}