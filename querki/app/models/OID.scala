package models

import anorm._
import play.api.db._
import play.api.Play.current

/**
 * By and large, we internally use ThingPtrs. This is an abstract "pointer" to a Thing.
 * It can be either an OID or a hard reference. (Note that Thing derives from ThingPtr.)
 * 
 * To be honest, this is a slightly premature optimization -- I don't know how much it
 * really matters. But given how much of the actual *processing* of Querki is going to
 * be chasing through networks of objects, I want to leave room for optimizing it. So
 * the notion is that we will probably eventually want two-phase loading. First, we
 * will load all of the objects in their "raw" form, with just OIDs. Once that's done,
 * we can do a second phase, resolving those OIDs to hard pointers, to make lookups
 * faster in processing.
 * 
 * So I'm introducing ThingPtr as an abstraction now, so that we can add that sort of
 * optimization later.
 */
//trait ThingPtr {
//  def id:OID
//}

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
 */
class OID(val raw:Long) {
  override def toString = java.lang.Long.toString(raw, 36)
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
  def next = {
    DB.withTransaction { implicit conn =>
      val nextQuery = SQL("""
          select * from OIDNexter
          """)
      val stream = nextQuery.apply()
      val localId =  stream.headOption.map(row => row.get[Int]("nextId").get).get
      SQL("""
          UPDATE OIDNexter SET nextId = {next} WHERE nextId = {old}
          """).on("next" -> (localId + 1).toString, "old" -> localId).executeUpdate()
      OID(1, localId)
    }
  }
  
  implicit def thing2OID(t:Thing):OID = t.id
}

object UnknownOID extends OID(-1)