package querki.test

import querki.globals._
import akka.actor._

class TestActor extends Actor {
  def receive = {
    case something => QLog.spew(s"TestActor got $something")
  }
}

/**
 * This is a place to put little, temporary tests. Note that these should normally be
 * marked ignore when checked in.
 * 
 * @author jducoeur
 */
class ScratchTests extends QuerkiTests
{
  "I" should {
    "be able to get a password" ignore {
      val Encryption = interface[querki.security.Encryption]
      
      val plaintext = "testing"
      val hash = Encryption.calcHash(plaintext)
      val auth = Encryption.authenticate(plaintext, hash)
      println(s"""The encrypted form of $plaintext is '$hash'; authenticated = $auth""")
    }
    
    "be able to identify an ActorRef as such" ignore {
      
      val anyRefClass = classOf[Object]
      def collectClasses(clazz:Class[_]):Set[Class[_]] = {
        def collectRec(sup:Class[_]):Set[Class[_]] = {
          if (sup == null || sup == anyRefClass)
            Set.empty
          else
            collectClasses(sup) + sup
        }
        
        (collectRec(clazz.getSuperclass) /: clazz.getInterfaces) { (set, interf) =>
          set ++ collectRec(interf)
        }
      }
      
      val actorSystem = ActorSystem()
      val ref = actorSystem.actorOf(Props(classOf[TestActor]))
      val clazz = ref.getClass
      QLog.spew(s"The ActorRef is actually a $clazz")
      QLog.spew(s"The full collection of superclasses are:")
      collectClasses(clazz).foreach { sup =>
        QLog.spew(s"    $sup")
      }
    }
  }
}

