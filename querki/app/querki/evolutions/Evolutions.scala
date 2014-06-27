package querki.evolutions

import models.OID

import steps._

import querki.ecology._
import querki.identity.UserId

object MOIDs extends EcotIds(29)

/**
 * The main API for Space Evolutions.
 * 
 * Evolutions deal with the issue of altering the schema of Spaces. We don't want to do that as a single
 * big push, because it would potentially take a devastating amount of time -- there are potentially
 * millions of tables to update. So instead, we focus on evolving each Space at load-time.
 * 
 * The high concept here is lifted from Play's evolution system. We have a number of individual
 * "steps", in numeric order. When we find that a Space is behind the current version number, we
 * run it through those steps to bring it up to the current time.
 * 
 * This currently runs synchronously while the Space is loading. That's not necessarily optimal:
 * it's putting a lot of dangerous work inside the Space's preload. In the medium term, we might
 * refactor Space load to be more step-wise, with evolution as an early step in that process. (This
 * would also allow us to show progress messages as it is happening.)
 * 
 * To add a Step, simply create it in querki.evolutions.steps (make sure its version is set!), and
 * add it to stepList below.
 */
class EvolutionsEcot(e:Ecology) extends QuerkiEcot(e) with Evolutions with UserEvolutions {
  // All Steps must be registered in this list.
  // NOTE: for the moment, we are *not* tolerant of gaps in the numeric sequence. We might need to
  // make this more robust at some point.
  private lazy val stepList:Seq[Step] = Seq(
    new Step2,
    new Step3,
    new Step4,
    new Step5
  )
  
  private lazy val steps:Map[Int, Step] = Map(stepList.map(step => (step.version, step)):_*)
  
  val currentVersion = steps.keys.max
      
  /**
   * Checks whether the provided Space (with the given version) is up to date; if not, it evolves
   * the Space to current spec.
   * 
   * IMPORTANT: this is potentially SLOW, with a lot of heavy database work going on. Make sure
   * you are running in a thread where that isn't going to be a huge problem! This is a static
   * method at the moment, so it runs in the calling thread. Eventually, we might make it into
   * its own Actor, but for the moment it runs under the Space Actor.
   */
  def checkEvolution(spaceId:OID, version:Int):Unit = {
    if (version < currentVersion) {
      val next = version + 1
      val step = steps(next)
      step.evolveUp(spaceId)
      checkEvolution(spaceId, next)
    }
  }
  
  /********************************************
   * User Evolutions
   * 
   * TBD: the code for User Evolution is awfully duplicative of that for Space Evolutions. We may want
   * to lift some abstractions out of this.
   ********************************************/
  
  private lazy val userStepList:Seq[UserStep] = Seq(
    new UserStep1
  )
  
  private lazy val userSteps:Map[Int, UserStep] = Map(userStepList.map(step => (step.version, step)):_*)
  
  lazy val currentUserVersion = userSteps.keys.max
  
  def checkUserEvolution(userId:UserId, version:Int):Unit = {
    if (version < currentUserVersion) {
      val next = version + 1
      val step = userSteps(next)
      step.evolveUp(userId)
      checkUserEvolution(userId, next)
    }
  }
}