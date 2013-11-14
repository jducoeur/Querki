package querki.spaces

import akka.actor._

import models.{OID}

import PersistMessages._

/**
 * This actor manages the actual persisting of a Space to and from the database. This code
 * was originally contained in the Space itself, but has been pulled out into its own Actor
 * so as to reduce blocking in the main Space Actor, and to abstract things away for testing.
 * 
 * *All* Space-specific code that talks to the database should go through here! That way,
 * we can unit-test Space itself without any DB dependencies, and we have a nice bottleneck
 * to replace if and when we change the persistence model.
 * 
 * Note the implication: this sucker does a *lot* of blocking, and asks to here are likely to
 * be bottlenecks. So use ask with caution, preferably using the Requester pattern. There is
 * one SpacePersister per Space, so they don't interfere with each other.
 * 
 * The Persister does not maintain its own State. Instead, it works hand-in-glove with the
 * Space itself to manage that.
 * 
 * Write requests are, by and large, best-effort. The medium-term plan is that, if this throws
 * any exceptions, it should result in killing the Space.
 */
private [spaces] class SpacePersister(val id:OID) extends Actor {

  def receive = {
    case Load => {
      // TODO: insert the actual code here
      
      // TODO: return the newly-loaded Space:
      sender ! Loaded(models.system.SystemSpace.State)
    }
  }
  
}