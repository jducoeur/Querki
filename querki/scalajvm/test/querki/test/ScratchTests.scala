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
class ScratchTests extends QuerkiTests {
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
      def collectClasses(clazz: Class[_]): Set[Class[_]] = {
        def collectRec(sup: Class[_]): Set[Class[_]] = {
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

    // This is mainly a test, to prove to myself that we need to know the actual type T
    // at the time we invoke the typeclass-using function; it can't be ambiguous.
    // Unfortunate, but as I suspected.
    // Since this is just a compiler test, we're ignoring it.
    "be able to invoke something with typeclasses" ignore {
      sealed trait Thingy
      case class ThingI(i: Int) extends Thingy
      case class ThingS(s: String) extends Thingy

      trait FooAble[T] {
        def foo(t: T): String
      }

      object MyFooAbles {
        implicit object FooAbleI extends FooAble[ThingI] {
          def foo(t: ThingI) = t.i.toString
        }
        implicit object FooAbleS extends FooAble[ThingS] {
          def foo(t: ThingS) = t.s
        }
      }

      object FooShower {
        def showIt[T : FooAble](t: T) = println(implicitly[FooAble[T]].foo(t))
      }

      import MyFooAbles._

      val thingies = List.empty[Thingy]
      val result = thingies.map {
        _ match {
          case i: ThingI => FooShower.showIt(i)
          case s: ThingS => FooShower.showIt(s)
        }
      }
    }
  }
}
