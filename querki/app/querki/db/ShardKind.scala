package querki.db

/**
 * The categories of database shards that we recognize. Any given Querki install must
 * have System and User shards defined. In production, all nodes point to the same
 * System shard; each node points to the User shard that it is working on.
 */
object ShardKind extends Enumeration {
  type ShardKind = Value
  
  val System, Test, User = Value
  
  /**
   * Fetch the configuration-system name of this shard.
   */
  def dbName(kind:ShardKind):String = {
    kind match {
      case System => "system"
      case Test => "test"
      case User => "user"
    }
  }
}