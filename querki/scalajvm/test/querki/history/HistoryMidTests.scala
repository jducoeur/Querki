package querki.history

import org.scalatest.tags.Slow
import org.scalatest.Matchers._
import querki.data.TID
import querki.test.mid._
import AllFuncs._
import HistoryMidFuncs._
import querki.security.SecurityMidFuncs._
import querki.test.mid.ClientState._

object HistoryMidTests {
  /**
   * This tests that undelete results in the expected _creator and create time.
   */
  lazy val testQIbu6oeer: TestOp[Unit] = {
    for {
      _ <- step("Regression test for QI.bu6oeer")
      std <- getStd
      owner <- setupUserAndSpace("QIbu6oeer Owner", "QIbu6oeer Space")
      spaceId <- TestOp.fetch(_.curSpace.info.oid)
      member = TestUser("QIbu6oeer Member")
      _ <- newUser(member)
      _ <- inviteIntoSpace(owner, spaceId, member)
      _ <- switchToUser(owner)

      // Give edit access to the member. This approach is a bit slimy, but it works, and is much
      // easier than following the way the client does it:
      editorRoleInfo <- getThingInfo(TID("Editor"))
      editorRoleTID = editorRoleInfo.oid
      _ <- grantRoleTo(member, editorRoleTID)

      model <- makeModel("The Model")

      // Note that the member creates the Instance:
      instance <- withUser(member) {
        makeThing(model, "The Instance")
      }
      creator <- evaluateQL(instance, "_creator -> _oid")
      createTime <- evaluateQL(instance, "_createTime")

      // Delete the Thing...
      _ <- deleteThing(instance)
      // ... then undelete it...
      info <- restoreDeletedThing(instance)
      rendered <- evaluateQL(instance, "____")
      // ... and check that the values match what they originally said:
      creator2 <- evaluateQL(instance, "_creator -> _oid")
      createTime2 <- evaluateQL(instance, "_createTime")
      _ <- testAssert(creator2 === creator)
      _ <- testAssert(createTime2 === createTime)
    }
      yield ()
  }

  /**
   * This tests the _undeleteThing function.
   */
  lazy val test7w4ger8: TestOp[Unit] = {
    for {
      _ <- step("Regression test for QI.7w4ger8")
      std <- getStd
      owner <- setupUserAndSpace("QI7w4ger8 Owner", "QI7w4ger8 Space")
      spaceId <- TestOp.fetch(_.curSpace.info.oid)

      model <- makeModel("The Model")
      instance <- makeThing(model, "The Instance")
      originalRendered <- evaluateQL(instance, "Name")

      // Get the OID, which we will need:
      oidWiki <- evaluateQL(instance, "_oid")
      oid = oidWiki.plaintext

      // Delete the Thing...
      _ <- deleteThing(instance)
      // ... then undelete it with the _undeleteThing function...
      info <- evaluateQL(spaceId, s"_undeleteThing($oid)")
      // ... and check that it looks correct:
      rendered <- evaluateQL(instance, "Name")
      _ <- testAssert(rendered.plaintext === originalRendered.plaintext)
    }
      yield ()
  }
}

@Slow
class HistoryMidTests extends MidTestBase {
  import HistoryMidTests._

  "HistoryMidTests" should {
    "pass" in {
      runTest(testQIbu6oeer)
      runTest(test7w4ger8)
    }
  }
}
