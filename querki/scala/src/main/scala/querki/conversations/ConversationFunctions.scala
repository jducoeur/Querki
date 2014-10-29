package querki.conversations

import scala.concurrent.Future

trait ConversationFunctions {
  /**
   * Fetch the Conversations for this Thing. Note that this may simply declare that you
   * don't have the right to read these Conversations.
   */
  def getConversationsFor(thingId:String):Future[ConversationInfo]
}
