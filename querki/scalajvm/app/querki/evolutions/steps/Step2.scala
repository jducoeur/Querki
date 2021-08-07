package querki.evolutions.steps

import anorm._
import play.api.db._

import querki.ecology._

import models._

import querki.evolutions._

/**
 * Step 2 is the first evolution defined, since "step" 1 is the pre-evolution code.
 *
 * This Step adds Modified timestamps for all Things.
 */
class Step2(implicit val ecology: Ecology) extends Step {
  val version = 2

  def doEvolve(info: SpaceInfo)(implicit conn: java.sql.Connection): Unit = {
    SpaceSQL(
      info.id,
      """
        ALTER TABLE {tname} ADD COLUMN modified TIMESTAMP
        """
    ).execute
    // NOTE: why is this necessary? For reasons beyond the ken of man, despite the fact that the above
    // (and every other version of DEFAULT I can think of) creates *new* objects correctly, the
    // added column winds up with an illegal 0 value for the *existing* rows, which immediately crashes.
    // So we need to set the value for the existing rows to something plausible and legal.
    SpaceSQL(
      info.id,
      """
        UPDATE {tname} SET modified = '2013-01-01 00:00:01'
        """
    ).execute
  }
}
