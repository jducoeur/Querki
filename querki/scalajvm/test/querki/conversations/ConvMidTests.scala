package querki.conversations

import org.scalatest.tags.Slow
import org.scalatest.Matchers._

import querki.test.mid._
import AllFuncs._

object ConvMidTests {
  /**
    * This test suite expects to be run in the context of a Space that has already been set up.
    */
  val convTests: TestOp[Unit] = {
    for {
      _ <- step("Create a Conversation")
      tid <- makeSimpleThing("ConvMidTests Thing")
      cmt1txt = "This is the text of the first comment"
      node1 <- startConversation(tid, cmt1txt)
      cmt2txt = "This is the first response in the first conversation"
      node2 <- addComment(tid, cmt2txt, node1)
      convs <- getConversationsFor(tid)
      convNode1 = convs.convs.head
      _ = assertCommentText(convNode1, cmt1txt)
      convNode2 = convNode1.responses.head
      _ = assertCommentText(convNode2, cmt2txt)

      _ <- step(s"Testing QI.bu6oe4b")
      authorHandle <- TestOp.fetch(_.testUser.handle)
      _ = convNode1.comment.author.handle should be (authorHandle)

      _ <- step("Delete a Comment")
      _ <- deleteComment(tid, node1)
      convs2 <- getConversationsFor(tid)
      convNode21 = convs2.convs.head
      _ = convNode21.comment.isDeleted should be (true)
      _ = assertCommentText(convNode21, commentDeletedText)
      convNode22 = convNode21.responses.head
      _ = convNode22.comment.isDeleted should be (false)
      _ = assertCommentText(convNode22, cmt2txt)
      
      _ <- step("Conversation Regression Tests")
      _ <- testQIbu6oehi
    }
      yield ()
  }
  
  /**
   * "_thingConversations does not render the QL in the comments"
   * 
   * We are testing this by including a link and making sure it renders.
   */
  val testQIbu6oehi: TestOp[Unit] = {
    for {
      _ <- step("Testing QI.bu6oehi")
      // Create the source thing, and a comment with some QL
      convTid <- makeSimpleThing("bu6oehi Comment Thing")
      rawQuery = s"[[${convTid.underlying}]]"
      cmt1txt = s"Referring back to myself: $rawQuery"
      node1 <- startConversation(convTid, cmt1txt)

      // Create the reader thing:
      view <- defaultView("The comments on the other Thing are [[bu6oehi Comment Thing -> _thingConversations]]")
      readTid <- makeSimpleThing("bu6oehi Read Thing", view)
      
      // Look at it and validate it:
      readPage <- getThingPage(readTid, None)
      pageWikitext = readPage.rendered.plaintext
      _ = pageWikitext shouldNot include (rawQuery)
      _ = pageWikitext should include ("""<a href="bu6oehi-Comment-Thing">bu6oehi Comment Thing</a>""")
    }
      yield ()
  }
}

@Slow
class ConvMidTests extends MidTestBase {
  "Conversations" should {
    "work" in {
      import ConvMidTests._

      val test = for {
        _ <- setupUserAndSpace("Conv Mid Test User", "Conv Mid Test Space")
        _ <- convTests
      }
        yield ()

      runTest(test)
    }
  }
}
