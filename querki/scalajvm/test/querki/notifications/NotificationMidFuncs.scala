package querki.notifications

import autowire._
import org.scalactic.source.Position
import org.scalatest.Matchers._
import querki.globals.execContext
import querki.test.mid.ClientState.withUser
import querki.test.mid._

trait NotificationMidFuncs {
  def getRecentNotifications(): TestOp[Seq[NotificationInfo]] =
    TestOp.client { _[NotificationFunctions].getRecentNotifications().call() }

  def numNewNotifications(): TestOp[Int] =
    TestOp.client { _[NotificationFunctions].numNewNotifications().call() }

  def readThrough(id: Common.NotificationId): TestOp[Unit] =
    TestOp.client { _[NotificationFunctions].readThrough(id).call() }

  /**
   * Marks all notifications as read for the current User.
   */
  def clearNotifications(): TestOp[Unit] = {
    for {
      notifies <- getRecentNotifications()
      highestId = notifies.foldLeft(-1) { (max, notify) =>
        scala.math.max(max, notify.id)
      }
      _ <- readThrough(highestId)
    }
      yield ()
  }

  def assertNumNotifications(who: TestUser, num: Int)(implicit pos: Position): TestOp[Unit] = {
    withUser(who) {
      for {
        nNotifies <- numNewNotifications()
        _ = nNotifies should be (num)
      }
        yield ()
    }
  }
}

object NotificationMidFuncs extends NotificationMidFuncs