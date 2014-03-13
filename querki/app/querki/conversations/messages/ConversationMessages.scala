package querki.conversations.messages

import models.{OID, ThingId}

import querki.conversations._
import querki.identity.User

/**
 * Note that ConversationMessages are never sent directly on their own; instead, they are wrapped in a
 * ConversationRequest, and routed through the Space. The subclasses of ConversationMessage are the
 * payloads of ConversationRequest.
 */
sealed trait ConversationMessage

/**
 * This should get a ThingConversations as its response.
 */
case class GetConversations(thing:OID) extends ConversationMessage
