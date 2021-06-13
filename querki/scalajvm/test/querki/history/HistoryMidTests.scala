package querki.history

import org.scalatest.tags.Slow
import org.scalatest.Matchers._
import querki.data.TID
import querki.test.mid._
import AllFuncs._
import HistoryMidFuncs._
import org.scalactic.source.Position
import querki.history.HistoryFunctions.{CreateSummary, DeleteSummary}
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
    } yield ()
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
    } yield ()
  }

  def getOIDFor(tid: TID): TestOp[String] = {
    for {
      oidWiki <- evaluateQL(tid, "_oid")
      oid = oidWiki.plaintext
    } yield oid
  }

  // This is a broad scenario test for _listDeletedThings:
  lazy val test7w4gerb: TestOp[Unit] = {
    for {
      info <- setupStandardRegressionTest("7w4gerb")
      spaceId = info.spaceId

      model <- makeModel("The Model")
      first <- makeThing(model, "First")
      second <- makeThing(model, "Second")
      secondOID <- getOIDFor(second)
      third <- makeThing(model, "Third")

      // Initially, the list is empty:
      _ <- assertNumDeletedThingsIs(0)

      // After we delete something, it shows up:
      _ <- deleteThing(second)
      _ <- assertNumDeletedThingsIs(1)
      result <- evaluateQL(spaceId, """_listDeletedThings(render = ""[[_oid]]"")""")
      _ = assert(secondOID == result.plaintext)

      // After we undelete it, it's empty again:
      _ <- evaluateQL(spaceId, s"_undeleteThing($secondOID)")
      _ <- assertNumDeletedThingsIs(0)

      // After we re-delete it, it only shows up once, not twice:
      _ <- deleteThing(second)
      _ <- assertNumDeletedThingsIs(1)
      result <- evaluateQL(spaceId, """_listDeletedThings(render = ""[[_oid]]"")""")
      _ = assert(secondOID == result.plaintext)

      // Create a second model and instance, and delete that:
      secondModel <- makeModel("Second Model")
      secondFirst <- makeThing(secondModel, "Second First")
      _ <- deleteThing(secondFirst)
      // Now we should see two deleted things:
      _ <- assertNumDeletedThingsIs(2)

      // But if we add a filter on the Model, we only see that one:
      result <- evaluateQL(
        spaceId,
        """_listDeletedThings(filter = _model -> _is(The Model), render = ""[[_oid]]"") -> _commas"""
      )
      _ = assert(result.plaintext.split(',').length == 1)
      _ = assert(secondOID == result.plaintext)
    } yield ()
  }

  def assertNumDeletedThingsIs(expected: Int)(implicit pos: Position): TestOp[Unit] = {
    for {
      spaceId <- TestOp.fetch(_.curSpace.info.oid)
      result <- evaluateQL(spaceId, """_listDeletedThings() -> _commas""")
      _ = assert(result.plaintext.split(',').filterNot(_.isEmpty).length == expected)
    } yield ()
  }

  // This tests that you can feed the results of _listDeletedThings() into _undeleteThing to undelete en masse:
  lazy val test7w4geta: TestOp[Unit] = {
    for {
      info <- setupStandardRegressionTest("7w4geta")
      spaceId = info.spaceId

      // Make some Things:
      model <- makeModel("The Model")
      first <- makeThing(model, "First")
      second <- makeThing(model, "Second")
      third <- makeThing(model, "Third")
      count <- evaluateQL(spaceId, "The Model._instances -> _count").map(_.plaintext).map(_.toInt)
      _ = assert(count == 3)

      // Delete them all:
      _ <- deleteThing(first)
      _ <- deleteThing(third)
      _ <- deleteThing(second)

      // Show that they are all deleted:
      _ <- assertNumDeletedThingsIs(3)
      count <- evaluateQL(spaceId, "The Model._instances -> _count").map(_.plaintext).map(_.toInt)
      _ = assert(count == 0)

      // Undelete them en masse:
      result <- evaluateQL(spaceId, """_listDeletedThings() -> _undeleteThing""")
      // And confirm that there are no longer any deleted Things:
      _ <- assertNumDeletedThingsIs(0)
      count <- evaluateQL(spaceId, "The Model._instances -> _count").map(_.plaintext).map(_.toInt)
      _ = assert(count == 3)

    } yield ()
  }

  lazy val testHistoryWalking: TestOp[Unit] = {
    for {
      info <- setupStandardRegressionTest("HistoryWalking")
      spaceId = info.spaceId

      // Make some Things:
      model <- makeModel("The Model")
      first <- makeThing(model, "First")
      second <- makeThing(model, "Second")
      third <- makeThing(model, "Third")
      count <- evaluateQL(spaceId, "The Model._instances -> _count").map(_.plaintext).map(_.toInt)
      _ = assert(count == 3)

      // Delete them all:
      _ <- deleteThing(first)
      _ <- deleteThing(third)
      _ <- deleteThing(second)

      // Show that they are all deleted:
      _ <- assertNumDeletedThingsIs(3)
      count <- evaluateQL(spaceId, "The Model._instances -> _count").map(_.plaintext).map(_.toInt)
      _ = assert(count == 0)

      // Fetch the history:
      fullHistory <- getHistorySummary(None, 100)
      // The last thing that happened should have been deleting the second Thing:
      lastEvent = fullHistory.events.last
      _ = assert(lastEvent.isInstanceOf[DeleteSummary])
      DeleteSummary(lastEventIdx, who, time, id) = lastEvent
      _ = assert(id == second)

      // Look back a couple of steps, and check that things looked as expected:
      afterFirstDeleted = lastEventIdx - 2
      _ <- withHistoryVersion(afterFirstDeleted) {
        for {
          instanceNames <- evaluateQL(spaceId, "The Model._instances -> Name -> _commas").map(_.plaintext)
          _ = assert(instanceNames == "Second, Third")
        } yield ()
      }

      // At the moment, there's nothing:
      instanceNames <- evaluateQL(spaceId, "The Model._instances -> Name -> _commas").map(_.plaintext)
      _ = assert(instanceNames == "")

      // Now actually roll back to that point, and check that the world has changed:
      _ <- rollbackTo(afterFirstDeleted)
      instanceNames <- evaluateQL(spaceId, "The Model._instances -> Name -> _commas").map(_.plaintext)
      _ = assert(instanceNames == "Second, Third")
    } yield ()
  }

  def instanceName(i: Int): String = s"Instance $i"

  def makeLotsOfInstances(
    model: TID,
    start: Int,
    nInstances: Int
  ): TestOp[Unit] = {
    (start until (start + nInstances)).foldLeft(TestOp.unit) { (to, i) =>
      to.flatMap { _ =>
        makeThing(model, instanceName(i)).map(_ => ())
      }
    }
  }

  lazy val testHistorySummaries: TestOp[Unit] = {
    for {
      info <- setupStandardRegressionTest("HistorySummaries")

      // Create some history:
      model <- makeModel("The Model")
      _ <- makeLotsOfInstances(model, 0, 100)

      // Okay, let's start at the most recent version. There are two history records per created Thing.
      blockSize = 10
      summary <- getHistorySummary(None, blockSize)
      first = summary.events.head
      CreateSummary(idx, who, time, kind, firstId, model, restored) = first
      firstInfo <- getThingInfo(firstId)
      _ = assert(firstInfo.linkName == Some(instanceName(95)))

      // Step back another set.
      summary <- getHistorySummary(Some(first.idx - 1), blockSize)
      first = summary.events.head
      CreateSummary(idx, who, time, kind, firstId, model, restored) = first
      firstInfo <- getThingInfo(firstId)
      _ = assert(firstInfo.linkName == Some(instanceName(90)))
    } yield ()
  }
}

@Slow
class HistoryMidTests extends MidTestBase {
  import HistoryMidTests._

  "HistoryMidTests" should {
    "pass" in {
      runTest(testQIbu6oeer)
      runTest(test7w4ger8)
      runTest(test7w4gerb)
      runTest(test7w4geta)
      runTest(testHistoryWalking)
      runTest(testHistorySummaries)
    }
  }
}
