package models

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
 * TODO: this should be a Value Class, but I can't do that until Play supports 2.10...
 */
class OID(val raw:Long) /*extends AnyVal*/ {
  override def toString = java.lang.Long.toString(raw, 36)
}

object OID {
  def apply(raw:Long) = new OID(raw)
  def apply(name:String) = new OID(java.lang.Long.parseLong(name))
  def apply(shard:Int, index:Int) = new OID(shard << 32 + index)
}