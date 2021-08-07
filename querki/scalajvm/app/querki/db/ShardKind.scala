package querki.db

/**
 * The categories of database shards that we recognize. Any given Querki install must
 * have System and User shards defined. In production, all nodes point to the same
 * System shard; each node points to the User shard that it is working on.
 */
object ShardKind extends Enumeration {
  type ShardKind = Value

  val System, Test, User, Template = Value

  /**
   * Fetch the configuration-system name of this shard. Pass the result of this into
   * DB.withConnection().
   */
  def dbName(kind: ShardKind): String = {
    kind match {
      case System   => "system"
      case Template => "template"
      case Test     => "test"
      case User     => "user"
    }
  }
}
