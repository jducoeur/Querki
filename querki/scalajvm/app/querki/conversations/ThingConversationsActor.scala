package querki.conversations

import akka.actor._
import akka.persistence._

import querki.globals._
import querki.persistence.PersistentQuerkiActor
import querki.types.ModelTypeDefiner
import querki.util.TimeoutChild

/**
 * This represents the active Conversations attached to a particular Thing. Note that essentially
 * all functionality comes from its base traits.
 */
class ThingConversationsActor(initState:SpaceState, thingId:OID, e:Ecology) 
  extends ThingConversationsCore(initState, thingId)(e) with PersistentQuerkiActor with TimeoutChild with ModelTypeDefiner 
{
  val timeoutConfig = "querki.conversations.timeout"
}

object ThingConversationsActor {
  def actorProps(initState:SpaceState, thingId:OID, e:Ecology) = Props(classOf[ThingConversationsActor], initState, thingId, e)
}
