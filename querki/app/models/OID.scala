package models

import language.implicitConversions

// TODO: this is all a very bad smell! OID needs to have an associated Ecot, which hides the
// details of creating the next OID, so that it can be properly stubbed out.
import anorm._
import play.api.db._
import play.api.Play.current

import querki.core.NameUtils
import querki.db.ShardKind._

/**
 * OID is the primary identifier for all objects in Querki. Internally, it is a long
 * integer, but it is usually represented externally (including many DB references) in
 * base 36, to keep it compact and readable. In general, it should be treated as an
 * opaque handle.
 * 
 * Technically speaking, OIDs are generated from the database shards. The upper 32 bits
 * are the index of a shard; the lower 32 are the index of this object's creation within the shard.
 * Note that objects are theoretically portable between shards, so you may *not* assume
 * that the upper 32 bits describe where it currently resides. This model is designed to
 * make it easy to distribute object creation, while having confidence that there won't
 * be overlap.
 * 
 * TODO: for Scala 2.11, change this to a Value Type (it is exactly the right use case for
 * those).
 */
class OID(val raw:Long) {
  override def toString = java.lang.Long.toString(raw, 36)
  def toThingId = AsOID(this)
  def id = this
  
  override def equals(other:Any) = {
    other match {
      case o:OID => this.raw == o.raw
      case _ => false
    }
  }
  override def hashCode = raw.hashCode
}

object OID {
  def apply(raw:Long) = new OID(raw)
  def apply(name:String) = new OID(java.lang.Long.parseLong(name, 36))
  def apply(shard:Int, index:Int) = new OID((shard.toLong << 32) + index)
  
  // TODO: this really ought to be done as a stored procedure, but let's wait until
  // we're using MySQL before we bother trying that. For now, we'll just use a
  // transaction -- inefficient, but it'll work.
  // TODO: eventually, this needs to become shard-smart. But that's a ways off yet.
  def next(kind:ShardKind) = {
    DB.withTransaction(dbName(kind)) { implicit conn =>
      val nextQuery = SQL("""
          select * from OIDNexter
          """)
      val stream = nextQuery.apply()
      val localId =  stream.headOption.map(row => row.get[Int]("nextId").get).get
      val shardId =  stream.headOption.map(row => row.get[Int]("shard").get).get
      SQL("""
          UPDATE OIDNexter SET nextId = {next} WHERE nextId = {old}
          """).on("next" -> (localId + 1).toString, "old" -> localId).executeUpdate()
      OID(shardId, localId)
    }
  }
  
  implicit def thing2OID(t:Thing):OID = t.id
  implicit def OID2ThingId(oid:OID):ThingId = oid.toThingId
}

object OIDMap {
  def apply[T <: Thing](items:T*):Map[OID, T] = {
    (Map.empty[OID,T] /: items) ((m, i) => m + (i.id -> i))
  }
}

object UnknownOID extends OID(-1)

/**
 * ThingId deals with the fact that we sometimes are passing OIDs around in messages, and
 * sometimes OIDs. This provides an abstraction that lets you use either in a sensible way.
 */
sealed trait ThingId {
  /**
   * This totally shouldn't be necessary, but Play apparently fails to properly escape Unicode *sometimes*,
   * so we have to do it by hand. Use this when your ThingId is getting rendered as "???????" in the URL that
   * shows up in the browser. (Initial evidence says that ThingId works fine from play templates, but fails
   * when used in Redirects from code.)
   */
  def encoded = java.net.URLEncoder.encode(toString, "UTF-8")
}
case class AsOID(oid:OID) extends ThingId {
  override def toString() = "." + oid.toString
}
case class AsName(name:String) extends ThingId {
  // We are currently using toUrl for this, *not* canonicalize, because we want to preserve case
  // in the URL. This does, however, mean that you can't casually compare the toString'ed versions
  // of two AsNames and assume that they are equal!
  override def toString() = NameUtils.toUrl(name)
}
// This is a bit of a hack, to work around certain cases where we need to *not* Urlify 
class AsDisplayName(name:String) extends AsName(name) {
  override def toString() = name
}
object UnknownThingId extends AsOID(UnknownOID)
object ThingId {
  def apply(str:String):ThingId = {
    str(0) match {
      case '.' => AsOID(OID(str.substring(1)))
      case _ => AsName(str)
    }
  }
  
  implicit def thingId2Str(id:ThingId) = id.toString()
}

case class IndexedOID(id:OID, i:Option[Int] = None) {
  override def toString = id.toString + i.map("[" + _.toString + "]").getOrElse("")
}

object IndexedOID {
  val regex = """([0-9a-zA-Z]+)(\[(\d+)\])?""".r
  def parse(str:String):Option[IndexedOID] = {
    str match {
      case regex(id, _, i) => {
        if (i == null)
          Some(IndexedOID(OID(id), None))
        else
          Some(IndexedOID(OID(id), Some(java.lang.Integer.parseInt(i))))
      }
      case _ => None
    }
  }
  
  implicit def OID2Indexed(id:OID):IndexedOID = IndexedOID(id)
}