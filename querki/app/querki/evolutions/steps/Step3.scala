package querki.evolutions.steps

import java.sql.Connection

import querki.spaces.Space.SpaceSQL

import querki.evolutions._

/**
 * This Step introduces the notion that Things can be deleted. Deletion is just a matter of
 * setting a flag, at least for now. It is *possible* that at some time in the future we
 * might actually clean up deleted Things, but for now we're not going to worry about them:
 * we don't care much about disk space, and they won't usually be loaded into memory.
 */
object Step3 extends Step {
  val version = 3
  
  def doEvolve(info:SpaceInfo)(implicit conn:java.sql.Connection):Unit = {
    SpaceSQL(info.id, """
        ALTER TABLE {tname} ADD COLUMN deleted BOOLEAN DEFAULT FALSE
        """).execute
  }
}