package querki.conversations

import scala.concurrent.Future

import messages._

import querki.data.TID

class ConversationFunctionsEmpty extends ConversationFunctions {
  def getConversationsFor(thingId:TID):Future[ConversationInfo] = ???
  def addComment(thingId:TID, text:String, responseTo:Option[CommentId]):Future[ConvNode] = ???
  def deleteComment(thingId:TID, commentId:CommentId):Future[Unit] = ???
}
