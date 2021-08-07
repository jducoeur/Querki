package models

/**
 * The in-memory representation of a database instance. Doesn't tend to matter much,
 * save to know where to look for things.
 *
 * TODO: wouldn't surprise me if Shards need their own Actors.
 */
class Shard(val id: Int) {}
