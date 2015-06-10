package querki

import scala.concurrent.Future

import models.Wikitext

import querki.ecology._
import querki.identity.{User, UserId}

package object admin {
  
  trait AdminOps extends EcologyInterface {
    /**
     * Fetches the current list of Spaces that are live in the system, and a little info about them.
     */
    def getSpacesStatus[B](req:User)(cb: SystemStatus => B):Future[B]
    
    /**
     * Publishes a System Message to all users.
     * 
     * This is fire-and-forget!
     */
    def sendSystemMessage(req:User, header:String, body:String):Unit
    
    /**
     * Fetch all the UserIds registered.
     * 
     * TODO: this method is stupid and deprecated. It will be replaced by something stream-oriented
     * as soon as I have a chance.
     */
    def getAllUserIds(req:User):Future[Seq[UserId]]
  }
}