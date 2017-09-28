package querki.conversations

import querki.globals._

class ConversationsEcot(e:Ecology) extends ClientEcot(e) {
  def implements = Set.empty
  
  lazy val Gadgets = interface[querki.display.Gadgets]
  
  override def postInit() = {
    Gadgets.registerGadget("._commentData", { CommentGadget.fromElem(_) })
    Gadgets.registerGadget("._conversationData", { ConversationGadget.fromElem(_) })
  }
}
