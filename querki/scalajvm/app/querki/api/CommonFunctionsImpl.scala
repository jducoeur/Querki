package querki.api

import scala.concurrent.Future

import models.Wikitext
import querki.data.ThingInfo
import querki.globals._
import querki.values.RequestContext

import CommonFunctions._

class PassthroughHandler(
  val ecology: Ecology,
  rc: RequestContext
) extends PassthroughHandlerBase
     with EcologyMember {
  val ClientApi = interface[ClientApi]
  val System = interface[querki.system.System]

  implicit val state = System.State

  var contents = Seq.empty[Future[(String, ThingInfo)]]

  def pass(name: String): ThingInfo = {
    state.anythingByName(name) match {
      case Some(t) => {
        // Technically, thingInfo() is async, but we expect it to be essentially synchronous in this case.
        // This is a bit of a bad smell, but essential to the way PassthroughHandler works.
        val tiFut = ClientApi.thingInfo(t, rc).map { (name -> _) }
        contents :+= tiFut
        // Okay, yes, this is kind of evil. But on this side we don't actually care about the returned
        // value, just what gets stuffed into contents, and this lets the common signature be what we want.
        null
      }
      case None => {
        throw new Exception(s"Attempting to send unknown Standard Thing $name")
      }
    }
  }
}

class CommonFunctionsImpl(info: AutowireParams)(implicit e: Ecology)
  extends AutowireApiImpl(info, e)
     with CommonFunctions {
  lazy val TermsOfService = interface[querki.system.TermsOfService]

  def doRoute(req: Request): Future[String] = route[CommonFunctions](this)(req)

  def getStandardThings(): Future[Map[String, ThingInfo]] = {
    val passthrough = new PassthroughHandler(ecology, rc)
    val translator = new StandardThings(passthrough)
    val toucher = translator.touchEverything()
    Future.sequence(passthrough.contents).map { seq =>
      seq.toMap
    }
  }

  def getProgress(handle: OperationHandle): Future[OperationProgress] = {
    handle match {
      case ActorOperationHandle(path) => {
        val selection = context.system.actorSelection(path)
        selection.requestFor[OperationProgress](ProgressActor.GetProgress)
      }
      case _ => throw new Exception(s"Received unknown OperationHandle $handle")
    }
  }

  def acknowledgeComplete(handle: OperationHandle): Unit = {
    handle match {
      case ActorOperationHandle(path) => {
        val selection = context.system.actorSelection(path)
        selection ! ProgressActor.CompletionAcknowledged
      }
      case _ => throw new Exception(s"Received unknown OperationHandle $handle")
    }
  }

  def fetchTOS(): Future[TOSInfo] = {
    val current = TermsOfService.currentTOS
    fut(TOSInfo(current.version, Wikitext(current.text)))
  }
}
